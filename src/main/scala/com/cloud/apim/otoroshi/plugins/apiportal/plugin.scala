package otoroshi_plugins.com.cloud.apim.plugins.apiportal

import otoroshi.next.models.*
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import org.joda.time.DateTime
import otoroshi.api.WriteAction
import otoroshi.cluster.ClusterAgent
import otoroshi.env.Env
import otoroshi.models.{ApiKey, PrivateAppsUser}
import otoroshi.next.plugins.api.*
import otoroshi.next.proxy.NgProxyEngineError
import otoroshi.security.IdGenerator
import otoroshi.utils.syntax.implicits.*
import otoroshi.utils.yaml.Yaml
import play.api.http.HttpEntity
import play.api.libs.json.*
import play.api.libs.ws.WSBodyWritables.given
import play.api.mvc.{Result, Results}

import java.net.{URI, URLDecoder}
import java.nio.charset.StandardCharsets
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.*

/**
 * TODO:
 *
 * - [x] changer sidebar par navigation qui représentera la bar de nav du haut avec dedans des "sidebar"
 * - [ ] ajouter un flag sur une ressource pour activer le templating
 * - [x] ajouter un flag sur une ressource pour activer la transformation redoc
 * - [x] ajouter une section footer ?
 * - [x] specification doit etre un tableau (specifications ou apis)
 *   - [x] donc il faut eventuellement une landing de choix
 *   - [x]a combiner avec le flag de transformation redoc
 * - [x] une api peut avoir plusieur openapi (par version)
 *   - [x] il va falloir une sorte de selecteur dans redoc
 * - [ ] handle search
 * - [x] handle footer
 * - [x] handle banner
 * - [x] tester (the client of the reference, through /api/_proxy)
 * - [x] Subscription
 *   - [x] API apikeys (crud)
 *   - [x] Page apikeys
 *
 */

extension (result: Result) {
  /** Wraps a plain `Result` into the `Either` expected by `NgBackendCall.callBackend`. */
  def asBackendCall: Either[NgProxyEngineError, BackendCallResponse] =
    BackendCallResponse(NgPluginHttpResponse.fromResult(result), None).right
}

extension (result: Future[Result]) {
  /** Same as `Result.asBackendCall`, lifted over the `Future` returned by every route handler. */
  def asBackendCall(using ec: ExecutionContext): Future[Either[NgProxyEngineError, BackendCallResponse]] =
    result.map(_.asBackendCall)
}

case class OtoroshiApiPortalConfig(prefix: Option[String], apiRef: Option[String]) extends NgPluginConfig {
  def json: JsValue = OtoroshiApiPortalConfig.format.writes(this)
}

object OtoroshiApiPortalConfig {
  val default = OtoroshiApiPortalConfig(None, None)
  val configFlow: Seq[String]        = Seq("prefix", "api_ref")
  val configSchema: Option[JsObject] = Some(
    Json.obj(
      "prefix" -> Json.obj(
        "type"  -> "string",
        "label" -> s"Path prefix"
      ),
      "api_ref" -> Json.obj(
        "type"  -> "select",
        "label" -> s"API",
        "props" -> Json.obj(
          "optionsFrom"        -> "/bo/api/proxy/apis/apis.otoroshi.io/v1/apis",
          "optionsTransformer" -> Json.obj(
            "label" -> "name",
            "value" -> "id"
          )
        )
      )
    )
  )
  given format: Format[OtoroshiApiPortalConfig] = new Format[OtoroshiApiPortalConfig] {
    override def reads(json: JsValue): JsResult[OtoroshiApiPortalConfig] = Try {
      OtoroshiApiPortalConfig(
        prefix = json.select("prefix").asOptString,
        apiRef = json.select("api_ref").asOptString,
      )
    } match {
      case Failure(e) => JsError(e.getMessage)
      case Success(e) => JsSuccess(e)
    }
    override def writes(o: OtoroshiApiPortalConfig): JsValue = Json.obj(
      "prefix"    -> o.prefix,
      "api_ref" -> o.apiRef,
    )
  }
}

/**
 * The documentations of the APIs, resolved: the ones loaded from a remote `source` kept a few seconds, not fetched again
 * for each page (a catalog shows many APIs at once).
 */
object DocumentationCache {
  private val ttl   = 15000L
  private val cache = new TrieMap[String, (Long, Option[ApiDocumentation])]()

  def resolve(api: Api)(using env: Env, ec: ExecutionContext): Future[Option[ApiDocumentation]] = api.documentation match {
    case Some(doc) if doc.source.isDefined =>
      val key = s"${api.id}:${Json.stringify(doc.json).hashCode}"
      val now = System.currentTimeMillis()
      cache.get(key) match {
        case Some((at, resolved)) if at > now - ttl => resolved.vfuture
        case _ =>
          api.resolveDocumentation().map { resolved =>
            cache.filterInPlace { case (_, (at, _)) => at > now - ttl }
            cache.put(key, (now, resolved))
            resolved
          }
      }
    case other => other.vfuture
  }
}

class OtoroshiApiPortal extends NgBackendCall {

  override def useDelegates: Boolean = false

  override def multiInstance: Boolean = true

  override def core: Boolean = true

  override def name: String = "Otoroshi API Portal"

  override def description: Option[String] = "This plugin serves a developer portal for an API: its documentation, its API reference with a client to try it, and the API keys of its developers.".some

  override def defaultConfigObject: Option[NgPluginConfig] = OtoroshiApiPortalConfig.default.some

  override def visibility: NgPluginVisibility = NgPluginVisibility.NgUserLand

  override def categories: Seq[NgPluginCategory] = Seq(NgPluginCategory.Other, NgPluginCategory.Custom("Cloud APIM"))

  override def steps: Seq[NgStep] = Seq(NgStep.CallBackend)

  override def noJsForm: Boolean = true

  override def configFlow: Seq[String] = OtoroshiApiPortalConfig.configFlow

  override def configSchema: Option[JsObject] = OtoroshiApiPortalConfig.configSchema

  override def callBackend(
                            ctx: NgbBackendCallContext,
                            delegates: () => Future[Either[NgProxyEngineError, BackendCallResponse]]
                          )(using
                            env: Env,
                            ec: ExecutionContext,
                            mat: Materializer
                          ): Future[Either[NgProxyEngineError, BackendCallResponse]] = {
    val config = ctx.cachedConfig(internalName)(OtoroshiApiPortalConfig.format).getOrElse(OtoroshiApiPortalConfig.default)
    config.apiRef match {
      case None => Results.NotFound("API ref not found").asBackendCall.vfuture
      case Some(ref) => env.datastores.apiDataStore.findById(ref) flatMap {
        case None => Results.NotFound("API not found").asBackendCall.vfuture
        case Some(api) => DocumentationCache.resolve(OtoroshiApiPortal.withExample(api)) flatMap {
            case Some(doc) if doc.enabled => OtoroshiApiPortal.route(Portal(api, doc, ctx, config)).asBackendCall
            case _ => Results.NotFound("API doc not found").asBackendCall.vfuture
          }
      }
    }
  }
}

object OtoroshiApiPortal {

  import PortalPages.*

  /** The security scheme the portal adds to the specifications: the API keys of the developers, as bearer tokens. */
  val securityScheme = "otoroshi_apikey"

  /** The built-in examples of documentation (the `doc_template` metadata of an API), to try the portal. */
  def withExample(api: Api): Api = api
    .applyOnIf(api.metadata.get("doc_template").contains("wines"))(_.copy(documentation = ApiDocumentationExample.wines.some))
    .applyOnIf(api.metadata.get("doc_template").contains("remote"))(_.copy(documentation = ApiDocumentationExample.remote.some))
    .applyOnIf(api.metadata.get("doc_template").contains("remote_test"))(_.copy(documentation = ApiDocumentationExample.remoteTest.some))
    .applyOnIf(api.metadata.get("doc_template").contains("remote_wines"))(_.copy(documentation = ApiDocumentationExample.remoteWines.some))
    .applyOnIf(api.metadata.get("doc_template").contains("test"))(_.copy(documentation = ApiDocumentationExample.value.some))

  def route(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    val method = portal.ctx.request.method.toLowerCase()
    // (an id in a path, as asked: not in lower case)
    def idIn(path: String): String = URLDecoder.decode(path.split("/").lift(3).getOrElse(""), StandardCharsets.UTF_8)
    (method, portal.path) match {
      case (_, "/api/_proxy")                                                    => serveProxy(portal)
      case ("get", "/api/session")                                               => serveSession(portal)
      case ("get", "/api/apikeys")                                               => signedIn(portal)(user => serveListApikeys(portal, user))
      case ("post", "/api/apikeys")                                              => guarded(portal)(user => serveCreateApikey(portal, user))
      case ("post", path) if path.startsWith("/api/apikeys/") && path.endsWith("/reveal") => guarded(portal)(user => serveRevealApikey(portal, user, idIn(portal.rawPath)))
      case ("put", path) if path.startsWith("/api/apikeys/")                    => guarded(portal)(user => serveUpdateApikey(portal, user, idIn(portal.rawPath)))
      case ("delete", path) if path.startsWith("/api/apikeys/")                 => guarded(portal)(user => serveDeleteApikey(portal, user, idIn(portal.rawPath)))
      case ("get", "/api/plans")                                                 => servePlansJson(portal)
      case ("get", "/api/documentation")                                         => serveDocumentationJson(portal)
      case ("get", "/api/openapi")                                               => serveOpenApi(portal)
      case ("get", path) if path.startsWith("/_portal/")                         => serveAsset(path.stripPrefix("/_portal/"))
      case ("get", "/keys") if portal.keysEnabled                                => Results.Ok(keysPage(portal)).as("text/html").vfuture
      case ("get", "/subscriptions" | "/subscriptions/apikeys")                  => Results.Redirect(s"${portal.prefix}/keys", 303).vfuture
      case ("get", "/api-references")                                            => serveReferences(portal)
      case ("get", path) if path.startsWith("/api-references/")                 => serveReference(portal, portal.rawPath.substring("/api-references".length))
      case ("get", "/login")                                                     => redirectAfterLogin(portal.ctx, s"${portal.prefix}/")
      case ("get", "/" | "")                                                     => serveHome(portal)
      case ("get", _)                                                            => serveResource(portal)
      case _                                                                     => Results.NotFound(Json.obj("error" -> "not found")).vfuture
    }
  }

  /* ---------- the pages ---------- */

  def handleRedirections(portal: Portal)(f: => Future[Result])(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    // (a path of the documentation: under the portal)
    val redirections = portal.doc.redirections.map(r => (r.from, portal.href(r.to))) :+ ("/logout" -> "/.well-known/otoroshi/logout")
    redirections.find(_._1.toLowerCase == portal.path) match {
      case Some((_, to)) => Results.Redirect(to, 303).vfuture
      case None => f
    }
  }

  // back where the developer signed in from (a path of this site only)
  def redirectAfterLogin(ctx: NgbBackendCallContext, home: String): Future[Result] = {
    val to = ctx.request.queryParam("redirect").filter(r => r.startsWith("/") && !r.startsWith("//") && !r.startsWith("/\\")).getOrElse(home)
    Results.Redirect(to, 303).vfuture
  }

  def serveAsset(name: String): Future[Result] = PortalAssets.files.get(name) match {
    case Some((bytes, contentType)) => Results.Ok(bytes).as(contentType).withHeaders("Cache-Control" -> "public, max-age=86400").vfuture
    case None => Results.NotFound("not found").vfuture
  }

  def serveHome(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = handleRedirections(portal) {
    val home = portal.doc.home
    (if (hasContent(home)) loadPage(home, portal.doc).map(Some(_)) else None.vfuture).map { content =>
      // (the title of the page is the one of the portal, said above it)
      val markdown = content.flatMap(_.markdown).map(_.replaceFirst("^\\s*#\\s+[^\\n]*\\n+", ""))
      Results.Ok(layout(portal, "home", None, homeBody(portal, content), markdown.map(m => Json.obj("markdown" -> m)).getOrElse(Json.obj()), markdown.isDefined)).as("text/html")
    }
  }

  def serveResource(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = handleRedirections(portal) {
    val doc  = portal.doc
    val path = portal.path
    (doc.resources ++ Seq(doc.logo)).find(_.path.exists(_.toLowerCase == path)) match {
      case None =>
        // a section of the navigation: its first page
        doc.navigation.find(_.path.exists(_.toLowerCase == path)).flatMap(nav => links(nav).headOption) match {
          case Some(first) => Results.Redirect(portal.href(first.link), 303).vfuture
          case None => Results.NotFound(notFoundPage(portal)).as("text/html").vfuture
        }
      case Some(resource) if resource.site_page && isReferenceTransform(resource) =>
        Results.Ok(referencePage(portal, resource.path.head, resource.title.getOrElse("API reference"))).as("text/html").vfuture
      case Some(resource) if resource.site_page => loadPage(resource, doc).map { content =>
        val nav = doc.navigation.find(n => links(n).exists(_.link.toLowerCase == path)).orElse(doc.navigation.find(_.path.exists(p => path.startsWith(p.toLowerCase))))
        val title = resource.title.orElse(nav.flatMap(n => links(n).find(_.link.toLowerCase == path).map(_.label)))
        val data = content.markdown.map(m => Json.obj("markdown" -> m)).getOrElse(Json.obj())
        val page = nav match {
          case Some(n) => layout(portal, "guide", title, guideBody(portal, n, content, path), data, content.markdown.isDefined)
          // (a page out of the navigation: on its own)
          case None => layout(portal, "page", title, s"""<div class="container"><article class="prose" id="content">${content.html}</article></div>""", data, content.markdown.isDefined)
        }
        Results.Ok(page).as("text/html")
      }
      case Some(resource) => fetchResource(resource, doc).map { case (status, body) => Results.Status(status)(body).as(resource.contentType) }
    }
  }

  def serveReferences(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = portal.doc.references match {
    case Seq() => Results.NotFound(notFoundPage(portal)).as("text/html").vfuture
    case Seq(ref) => Results.Ok(referencePage(portal, ref.link, ref.title)).as("text/html").vfuture
    case _ => Results.Ok(referencesPage(portal)).as("text/html").vfuture
  }

  def serveReference(portal: Portal, link: String)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    portal.doc.references.find(_.link.equalsIgnoreCase(link)) match {
      case Some(ref) => Results.Ok(referencePage(portal, ref.link, ref.title)).as("text/html").vfuture
      case None => Results.NotFound(notFoundPage(portal)).as("text/html").vfuture
    }
  }

  /** A resource of the documentation, as it is (from its url, or its content), with the status of its source. */
  def fetchResource(resource: ApiDocumentationResource, doc: ApiDocumentation)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[(Int, ByteString)] = {
    resource.resolveUrl(doc) match {
      case Some(url) => env.Ws.url(url)
        .withFollowRedirects(resource.httpFollowRedirects)
        .withHttpHeaders(resource.httpHeaders.toSeq*)
        .withRequestTimeout(resource.httpTimeout)
        .get()
        .map(resp => (resp.status, resp.bodyAsBytes))
        .recover { case e => (502, ByteString(e.getMessage)) }
      case None => (200, resource.base64_content
        .orElse(resource.json_content.map(_.stringify.byteString))
        .orElse(resource.text_content.map(ByteString(_)))
        .getOrElse(ByteString("no content"))).vfuture
    }
  }

  def loadResource(resource: ApiDocumentationResource, doc: ApiDocumentation)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[ByteString] =
    fetchResource(resource, doc).map(_._2)

  /** A page of the documentation: what its source answered, or why it could not be read. */
  def loadPage(resource: ApiDocumentationResource, doc: ApiDocumentation)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[PageContent] =
    fetchResource(resource, doc).map {
      case (status, body) if status < 400 => pageContent(resource, body)
      case (status, _) => PageContent(s"""<p class="muted">This page could not be loaded: its source answered $status.</p>""", None)
    }

  /* ---------- the specifications ---------- */

  // the specifications the reference shows: the references, and the resources rendered as one
  def specLinks(portal: Portal): Seq[String] =
    portal.doc.references.map(_.link) ++ portal.doc.resources.filter(isReferenceTransform).flatMap(_.path.headOption)

  /** A specification of the portal, in json (written in json or yaml), and where it comes from. */
  def loadSpec(portal: Portal, link: String)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Option[(JsObject, Option[String])]] = {
    val doc = portal.doc
    val loaded: Future[Option[(ByteString, Option[String])]] = doc.resources.find(_.path.exists(_.equalsIgnoreCase(link))) match {
      case Some(resource) => loadResource(resource, doc).map(body => Some((body, resource.resolveUrl(doc))))
      case None if link.startsWith("https://") || link.startsWith("http://") =>
        env.Ws.url(link).withRequestTimeout(30.seconds).get().map(resp => Some((resp.bodyAsBytes, Some(link))))
      case None => None.vfuture
    }
    loaded.map(_.flatMap { case (body, from) =>
      val text = body.utf8String
      Try(Json.parse(text)).toOption.orElse(Yaml.parse(text)).collect { case o: JsObject => (o, from) }
    }).recover { case _ => None }
  }

  /**
   * A specification as the reference shows it: the gateway of Otoroshi as its first server (the requests tried go
   * through it, with the key of the developer), the keys of the developers as its security scheme, and its relative
   * servers made absolute (the document is read from the portal, not from where it comes from).
   */
  def patchSpec(portal: Portal, spec: JsObject, from: Option[String]): JsObject = {
    if (spec.select("openapi").asOpt[String].isEmpty) spec else {
      val absolute = (url: String) => from.filter(_ => !url.matches("^[a-zA-Z][a-zA-Z0-9+.-]*://.*")).flatMap(base => Try(new URI(base).resolve(url).toString).toOption).getOrElse(url)
      val servers = spec.select("servers").asOpt[Seq[JsObject]].getOrElse(Seq.empty).map { s =>
        s.select("url").asOpt[String].map(url => s ++ Json.obj("url" -> absolute(url))).getOrElse(s)
      }
      val gateway = portal.gatewayUrl.filter(_ => portal.settings.gatewayServer).filterNot(gw => servers.exists(_.select("url").asOpt[String].contains(gw)))
      val withServers = spec ++ Json.obj("servers" -> (gateway.map(gw => Json.obj("url" -> gw, "description" -> "Through the Otoroshi gateway")).toSeq ++ servers))
      if (!portal.keysEnabled) withServers else {
        val requirement = Json.obj(securityScheme -> Json.arr())
        val components = withServers.select("components").asOpt[JsObject].getOrElse(Json.obj())
        val schemes = components.select("securitySchemes").asOpt[JsObject].getOrElse(Json.obj())
        val withScheme = (s: Seq[JsObject]) => if (s.exists(_.keys.contains(securityScheme))) s else requirement +: s
        // the operations that ask for their own security: the key of the developer as one of their choices
        val paths = withServers.select("paths").asOpt[JsObject].map { paths =>
          JsObject(paths.value.map {
            case (path, item: JsObject) => path -> JsObject(item.value.map {
              case (method, op: JsObject) => op.select("security").asOpt[Seq[JsObject]] match {
                case Some(security) if security.nonEmpty => method -> (op ++ Json.obj("security" -> withScheme(security)))
                case _ => method -> op
              }
              case other => other
            })
            case other => other
          })
        }
        withServers ++ Json.obj(
          "components" -> (components ++ Json.obj("securitySchemes" -> (schemes ++ Json.obj(securityScheme -> Json.obj(
            "type" -> "http",
            "scheme" -> "bearer",
            "description" -> "Your API key, from **My keys** on this portal: `Authorization: Bearer <your key>`"
          ))))),
          "security" -> withScheme(withServers.select("security").asOpt[Seq[JsObject]].getOrElse(Seq.empty))
        ) ++ paths.map(p => Json.obj("paths" -> p)).getOrElse(Json.obj())
      }
    }
  }

  def serveOpenApi(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    portal.ctx.request.queryParam("link").filter(specLinks(portal).contains) match {
      case None => Results.NotFound(Json.obj("error" -> "no such specification")).vfuture
      case Some(link) => loadSpec(portal, link).map {
        case None => Results.NotFound(Json.obj("error" -> "the specification could not be read"))
        case Some((spec, from)) => Results.Ok(patchSpec(portal, spec, from)).withHeaders("Cache-Control" -> "no-store")
      }
    }
  }

  /* ---------- the requests of the reference ---------- */

  private val allowedHostsCache = new TrieMap[String, (Long, Set[String])]()

  private def hostOf(url: String): Option[String] =
    Try(new URI(url)).toOption.filter(u => u.getScheme == "http" || u.getScheme == "https").flatMap(u => Option(u.getHost)).map(_.toLowerCase)

  /** Where the requests tried from the reference may go: the gateway of the API, the servers of its specifications. */
  def allowedHosts(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Set[String]] = {
    val key = s"${portal.api.id}:${portal.gatewayUrl.getOrElse("")}"
    allowedHostsCache.get(key) match {
      case Some((at, hosts)) if at > System.currentTimeMillis() - 60000L => hosts.vfuture
      case _ =>
        Future.sequence(specLinks(portal).map(link => loadSpec(portal, link))).map { specs =>
          val fromSpecs = specs.flatten.flatMap { case (spec, from) =>
            val patched = patchSpec(portal, spec, from)
            patched.select("servers").asOpt[Seq[JsObject]].getOrElse(Seq.empty).flatMap(_.select("url").asOpt[String]).flatMap(hostOf) ++
              spec.select("host").asOpt[String].map(_.split(":").head.toLowerCase)
          }
          val hosts = (portal.gatewayUrl.flatMap(hostOf).toSeq ++ fromSpecs).toSet
          allowedHostsCache.put(key, (System.currentTimeMillis(), hosts))
          hosts
        }
    }
  }

  // what the browser adds on its own (the cookies of the portal first), never sent to the API
  private val droppedRequestHeaders = Set(
    "host", "cookie", "connection", "keep-alive", "proxy-authenticate", "proxy-authorization", "te", "trailer", "trailers",
    "transfer-encoding", "upgrade", "content-length", "accept-encoding", "origin", "referer", "forwarded", "x-forwarded-for",
    "x-forwarded-host", "x-forwarded-proto", "x-forwarded-port", "x-real-ip", "x-scalar-cookie", "x-otoroshi-api-portal",
    "remote-address", "timeout-access", "raw-request-uri", "tls-session-info", "otoroshi-state", "otoroshi-claim",
    "otoroshi-request-id", "otoroshi-proxied-host", "otoroshi-request-timestamp"
  )
  // what would apply to the portal itself
  private val droppedResponseHeaders = Set(
    "content-type", "content-length", "content-encoding", "transfer-encoding", "connection", "keep-alive", "set-cookie",
    "strict-transport-security", "alt-svc"
  )

  /**
   * The requests tried from the reference (Scalar, `proxyUrl`): sent from the portal, so the browser asks no CORS, to the
   * hosts of the API only, without the cookies of the portal (`X-Scalar-Cookie` is the one the developer set).
   */
  def serveProxy(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    val ctx = portal.ctx
    ctx.request.queryParam("scalar_url").flatMap(url => hostOf(url).map(host => (url, host))) match {
      case None => Results.BadRequest(Json.obj("error" -> "the address of the request is missing or not http(s)")).vfuture
      case Some((url, host)) => allowedHosts(portal).flatMap { hosts =>
        if (!hosts.contains(host)) {
          Results.Forbidden(Json.obj("error" -> s"the portal sends requests to the hosts of the API only, not to $host")).vfuture
        } else {
          val headers = ctx.rawRequest.headers.headers.filterNot { case (name, _) =>
            val lower = name.toLowerCase
            droppedRequestHeaders.contains(lower) || lower.startsWith("sec-")
          } ++ ctx.rawRequest.headers.get("X-Scalar-Cookie").map(c => "Cookie" -> c).toSeq :+ ("Accept-Encoding" -> "identity")
          ctx.request.body.runFold(ByteString.empty)(_ ++ _).flatMap { body =>
            env.Ws.url(url)
              .withFollowRedirects(false)
              .withRequestTimeout(30.seconds)
              .withMethod(ctx.request.method.toUpperCase())
              .withHttpHeaders(headers*)
              .applyOnIf(body.nonEmpty)(_.withBody(body))
              .execute()
              .map { resp =>
                val respHeaders = resp.headers.toSeq.flatMap { case (name, values) => values.map(v => (name, v)) }.filterNot { case (name, _) => droppedResponseHeaders.contains(name.toLowerCase) }
                val bytes = resp.bodyAsBytes
                val entity = if (bytes.isEmpty) HttpEntity.NoEntity else HttpEntity.Strict(bytes, resp.header("Content-Type").orElse(Some("application/octet-stream")))
                Results.Status(resp.status).sendEntity(entity).withHeaders(respHeaders*)
              }
              .recover { case e => Results.BadGateway(Json.obj("error" -> s"the request could not be sent: ${e.getMessage}")) }
          }
        }
      }
    }
  }

  /* ---------- the json of the portal ---------- */

  def servePlansJson(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    Results.Ok(JsArray(portal.plans.map(planJson))).vfuture
  }

  def serveDocumentationJson(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    val api = portal.api
    val doc = portal.doc
    // (never the headers the resources are fetched with: they may hold credentials)
    val json = doc.json.asObject - "source" ++ Json.obj("resources" -> JsArray(doc.resources.map(_.raw - "http_headers")))
    Results.Ok(json ++ Json.obj(
      "name" -> api.name,
      "description" -> api.description,
      "api_tags" -> api.tags,
      "api_metadata" -> api.metadata,
      "doc_tags" -> doc.tags,
      "doc_metadata" -> doc.metadata,
    )).vfuture
  }

  def serveSession(portal: Portal)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    Results.Ok(Json.obj(
      "user" -> portal.user.map(u => Json.obj("name" -> u.name, "email" -> u.email)),
      "plans" -> JsArray(portal.keyPlans.map(planJson)),
      "login_url" -> s"${portal.rootPrefix}/login",
      "logout_url" -> portal.logoutUrl,
      "per_user" -> portal.settings.keysPerUser,
    )).withHeaders("Cache-Control" -> "no-store").vfuture
  }

  /* ---------- the keys of the developers ---------- */

  /** A key of a developer: their subscription to a plan, and the api key it gave (none while it waits an approval). */
  case class UserKey(sub: ApiSubscription, key: Option[ApiKey], plan: ApiPlan) {
    def waiting: Boolean = sub.status == ApiSubscriptionPending || key.isEmpty
    def enabled: Boolean = !waiting && sub.status != ApiSubscriptionDisabled && key.exists(_.enabled)
    def name: String = sub.metadata.get("otoroshi-api-client-name").orElse(key.map(_.clientName)).getOrElse(sub.name)
    def json: JsObject = Json.obj(
      "id" -> sub.id,
      "sub" -> sub.id,
      "client_id" -> key.map(_.clientId),
      "name" -> name,
      "description" -> (if (sub.description.nonEmpty) sub.description else key.map(_.description).getOrElse("")),
      "enabled" -> enabled,
      "plan" -> plan.id,
      "plan_name" -> plan.name,
      "status" -> (if (waiting) "waiting-approval" else "validated"),
      "created_at" -> sub.dates.created_at.getMillis,
    )
  }

  private def keyOf(sub: ApiSubscription)(using env: Env, ec: ExecutionContext): Future[Option[Option[ApiKey]]] = {
    sub.tokenRefs.flatMap(_.select("apikey").asOpt[String]).headOption match {
      case None => Some(None).vfuture
      // (a key gone: the subscription with it)
      case Some(id) => env.datastores.apiKeyDataStore.findById(id).map(_.map(Some(_)))
    }
  }

  def userKeys(portal: Portal, user: PrivateAppsUser)(using env: Env, ec: ExecutionContext): Future[Seq[UserKey]] = {
    val plans = portal.api.plans.map(p => p.id -> p).toMap
    val subs = env.proxyState.allApiSubscriptions().filter { s =>
      s.apiRef == portal.api.id && s.ownerRef == user.email && s.subscriptionKind == ApiKind.Apikey && plans.contains(s.planRef)
    }
    Future.sequence(subs.map(sub => keyOf(sub).map(_.map(key => UserKey(sub, key, plans(sub.planRef))))))
      .map(_.flatten.sortBy(_.sub.dates.created_at.getMillis))
  }

  /** A key of the developer, by the id of its subscription (or the client id of its api key). */
  def findUserKey(portal: Portal, user: PrivateAppsUser, id: String)(using env: Env, ec: ExecutionContext): Future[Option[UserKey]] = {
    env.datastores.apiSubscriptionDataStore.findById(id).flatMap {
      case Some(sub) => Some(sub).vfuture
      case None => env.datastores.apiKeyDataStore.findById(id).flatMap {
        case Some(key) => key.apiRef.map(_.subscription).orElse(key.metadata.get("otoroshi-api-sub-ref")) match {
          case Some(subId) => env.datastores.apiSubscriptionDataStore.findById(subId)
          case None => None.vfuture
        }
        case None => None.vfuture
      }
    }.flatMap {
      case Some(sub) if sub.apiRef == portal.api.id && sub.ownerRef == user.email =>
        portal.api.plans.find(_.id == sub.planRef) match {
          case None => None.vfuture
          case Some(plan) => keyOf(sub).map(_.map(key => UserKey(sub, key, plan)))
        }
      case _ => None.vfuture
    }
  }

  private def jsonError(status: Results.Status, message: String): Future[Result] = status(Json.obj("error" -> message)).vfuture

  private def signedIn(portal: Portal)(f: PrivateAppsUser => Future[Result]): Future[Result] = portal.user match {
    case None => jsonError(Results.Unauthorized, "sign in first")
    case Some(user) => f(user)
  }

  // the writes: from the pages of the portal only (a header a foreign form never sends)
  private def guarded(portal: Portal)(f: PrivateAppsUser => Future[Result]): Future[Result] = {
    if (!portal.ctx.rawRequest.headers.get("X-Otoroshi-Api-Portal").contains("1")) jsonError(Results.Forbidden, "the portal only")
    else signedIn(portal)(f)
  }

  private def bodyOf(portal: Portal)(using mat: Materializer, ec: ExecutionContext): Future[JsValue] =
    portal.ctx.request.body.runFold(ByteString.empty)(_ ++ _).map(b => Try(Json.parse(b.utf8String)).getOrElse(Json.obj()))

  private def saveKey(key: ApiKey)(using env: Env, ec: ExecutionContext): Future[Boolean] = {
    if (env.clusterConfig.mode.isWorker) {
      ClusterAgent.clusterSaveApikey(env, key)(using ec, env.otoroshiMaterializer)
    }
    env.datastores.apiKeyDataStore.set(key)
  }

  def serveListApikeys(portal: Portal, user: PrivateAppsUser)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    userKeys(portal, user).map(keys => Results.Ok(JsArray(keys.map(_.json))).withHeaders("Cache-Control" -> "no-store"))
  }

  /**
   * A new key: a subscription to the plan, its api key made by Otoroshi from the plan. A plan validated by hand: the
   * subscription waits (pending), its key made disabled, enabled when the subscription is confirmed.
   */
  def serveCreateApikey(portal: Portal, user: PrivateAppsUser)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    val api = portal.api
    bodyOf(portal).flatMap { body =>
      val name = body.select("name").asOpt[String].map(_.trim.take(80)).filter(_.nonEmpty).getOrElse("My key")
      val description = body.select("description").asOpt[String].map(_.trim.take(500)).getOrElse("")
      val planId = body.select("plan").asOpt[String]
      portal.keyPlans.find(p => planId.contains(p.id)) match {
        case None => jsonError(Results.BadRequest, "no such plan on this portal")
        case Some(plan) => userKeys(portal, user).flatMap { keys =>
          if (portal.settings.keysPerUser.exists(keys.size >= _)) {
            jsonError(Results.BadRequest, s"${portal.settings.keysPerUser.get} keys at most: revoke one to take another")
          } else {
            val now = DateTime.now()
            val auto = plan.validation.isAuto
            val sub = ApiSubscription(
              location = api.location,
              id = s"api-plan-subscription_${IdGenerator.uuid}",
              name = name,
              description = description,
              status = if (auto) ApiSubscriptionEnabled else ApiSubscriptionDisabled,
              dates = ApiSubscriptionDates(created_at = now, processed_at = now, started_at = now, paused_at = now, ending_at = now, closed_at = now),
              ownerRef = user.email,
              apiRef = api.id,
              subscriptionKind = ApiKind.Apikey,
              tokenRefs = Seq.empty,
              planRef = plan.id,
              tags = Seq.empty,
              metadata = Map(
                "created_by" -> "otoroshi-api-portal-plugin",
                "otoroshi-api-client-name" -> name,
              )
            )
            ApiSubscription.validate(api.id, sub, WriteAction.Create, isDraft = false).flatMap {
              case Left(error) => jsonError(Results.BadRequest, error)
              case Right(created) =>
                val saved = created.copy(status = if (auto) ApiSubscriptionEnabled else ApiSubscriptionPending)
                env.datastores.apiSubscriptionDataStore.set(saved).flatMap { _ =>
                  keyOf(saved).flatMap {
                    case Some(Some(key)) =>
                      // (the name the developer gave, unless the plan names its keys itself)
                      val named = if (plan.apikeyTemplate.clientNamePattern.isDefined) key.copy(description = description) else key.copy(clientName = name, description = description)
                      saveKey(named).map(_ => Some(named))
                    case _ => None.vfuture
                  }.map { key =>
                    val userKey = UserKey(saved, key, plan)
                    Results.Ok(userKey.json ++ (if (userKey.waiting) Json.obj() else Json.obj("bearer" -> key.map(_.toBearer()))))
                  }
                }
            }
          }
        }
      }
    }
  }

  def serveRevealApikey(portal: Portal, user: PrivateAppsUser, id: String)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    findUserKey(portal, user, id).flatMap {
      case Some(userKey) if !userKey.waiting => Results.Ok(userKey.json ++ Json.obj("bearer" -> userKey.key.map(_.toBearer()))).withHeaders("Cache-Control" -> "no-store").vfuture
      case Some(_) => jsonError(Results.BadRequest, "this key waits an approval")
      case None => jsonError(Results.NotFound, "no such key")
    }
  }

  def serveUpdateApikey(portal: Portal, user: PrivateAppsUser, id: String)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    bodyOf(portal).flatMap { body =>
      findUserKey(portal, user, id).flatMap {
        case None => jsonError(Results.NotFound, "no such key")
        case Some(userKey) =>
          val name = body.select("name").asOpt[String].map(_.trim.take(80)).filter(_.nonEmpty).getOrElse(userKey.name)
          val description = body.select("description").asOpt[String].map(_.trim.take(500)).getOrElse(userKey.sub.description)
          // (a key waiting an approval: never enabled by its developer)
          val enabled = body.select("enabled").asOpt[Boolean].filter(_ => !userKey.waiting)
          val sub = userKey.sub.copy(
            name = name,
            description = description,
            status = enabled.map(e => if (e) ApiSubscriptionEnabled else ApiSubscriptionDisabled).getOrElse(userKey.sub.status),
            metadata = userKey.sub.metadata + ("otoroshi-api-client-name" -> name)
          )
          val key = userKey.key.map { k =>
            val named = if (userKey.plan.apikeyTemplate.clientNamePattern.isDefined) k else k.copy(clientName = name)
            named.copy(description = description, enabled = enabled.getOrElse(k.enabled))
          }
          for {
            _ <- env.datastores.apiSubscriptionDataStore.set(sub)
            _ <- key.map(saveKey).getOrElse(true.vfuture)
          } yield Results.Ok(UserKey(sub, key, userKey.plan).json)
      }
    }
  }

  def serveDeleteApikey(portal: Portal, user: PrivateAppsUser, id: String)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    findUserKey(portal, user, id).flatMap {
      case None => jsonError(Results.NotFound, "no such key")
      case Some(userKey) =>
        val keys = userKey.sub.tokenRefs.flatMap(_.select("apikey").asOpt[String])
        if (env.clusterConfig.mode.isWorker) {
          keys.foreach(k => ClusterAgent.clusterDeleteApikey(env, k)(using ec, env.otoroshiMaterializer))
        }
        for {
          _ <- Future.sequence(keys.map(k => env.datastores.apiKeyDataStore.delete(k)))
          _ <- env.datastores.apiSubscriptionDataStore.delete(userKey.sub.id)
        } yield Results.NoContent
    }
  }
}
