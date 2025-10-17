package otoroshi_plugins.com.cloud.apim.plugins.apiportal

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import next.models._
import org.joda.time.DateTime
import otoroshi.cluster.ClusterAgent
import otoroshi.env.Env
import otoroshi.models.{ApiIdentifier, ApiKey, EntityLocation}
import otoroshi.next.plugins.api._
import otoroshi.next.proxy.NgProxyEngineError
import otoroshi.security.IdGenerator
import otoroshi.utils.syntax.implicits._
import play.api.libs.json._
import play.api.mvc.{Result, Results}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util._

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
 * - [ ] handle footer
 * - [ ] handle banner
 * - [ ] tester
 *   - API
 *   - Page
 * - Subscription
 *   - API apikeys (crud)
 *   - Page apikeys
 *
 */


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
  val format = new Format[OtoroshiApiPortalConfig] {
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

class OtoroshiApiPortal extends NgBackendCall {

  override def useDelegates: Boolean = false

  override def multiInstance: Boolean = true

  override def core: Boolean = true

  override def name: String = "Otoroshi API Portal"

  override def description: Option[String] = "This plugin serve some kind of simple API dev portal.".some

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
                          )(implicit
                            env: Env,
                            ec: ExecutionContext,
                            mat: Materializer
                          ): Future[Either[NgProxyEngineError, BackendCallResponse]] = {
    val config = ctx.cachedConfig(internalName)(OtoroshiApiPortalConfig.format).getOrElse(OtoroshiApiPortalConfig.default)
    config.apiRef match {
      case None => BackendCallResponse(NgPluginHttpResponse.fromResult(Results.NotFound("API ref not found")), None).rightf
      case Some(ref) => env.datastores.apiDataStore.findById(ref) flatMap {
        case None => BackendCallResponse(NgPluginHttpResponse.fromResult(Results.NotFound("API not found")), None).rightf
        case Some(api) => api.documentation match {
          case Some(doc) if doc.enabled => {
            val prefix = config.prefix.getOrElse("")
            val path = ctx.request.path.replaceFirst(prefix, "").toLowerCase()
            (ctx.request.method.toLowerCase(), path) match {
              case ("put", path) if path.startsWith("/api/apikeys/") => OtoroshiApiPortal.serveUpdateApikey(api, path.replaceFirst("/api/apikeys/", ""), ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("delete", path) if path.startsWith("/api/apikeys/") => OtoroshiApiPortal.serveDeleteApikey(api, path.replaceFirst("/api/apikeys/", ""), ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("post", "/api/apikeys") => OtoroshiApiPortal.serveCreateApikey(api, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get",  "/api/apikeys") => OtoroshiApiPortal.serveAllApikeys(api, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("post", "/api/_test") => OtoroshiApiPortal.serveApiTester(api, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", "/subscriptions/apikeys") => OtoroshiApiPortal.serveApikeysPage(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", "/subscriptions") => OtoroshiApiPortal.serveApikeysPage(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", "/tester") => OtoroshiApiPortal.serveTesterPage(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", "/portal.js") => OtoroshiApiPortal.serverJs(api, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", path) if path.startsWith("/api-references/") => OtoroshiApiPortal.serveApiDoc(api, doc, Some(path.replaceFirst("/api-references", "")), ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", "/api-references") => OtoroshiApiPortal.serveApiDoc(api, doc, None, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", "/login") => OtoroshiApiPortal.redirectToHome(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", "/") => OtoroshiApiPortal.serveHome(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", "") => OtoroshiApiPortal.serveHome(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
              case ("get", path) => OtoroshiApiPortal.serveResource(api, path, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
            }
          }
          case _ => BackendCallResponse(NgPluginHttpResponse.fromResult(Results.NotFound("API doc not found")), None).rightf
        }
      }
    }
  }
}

object OtoroshiApiPortal {
  def apikeysFromApiForUser(consumer: ApiConsumer, ctx: NgbBackendCallContext)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Seq[(ApiConsumerSubscription, ApiKey)]] = {
    Source(consumer.subscriptions.toList)
      .mapAsync(1)(ref => env.datastores.apiConsumerSubscriptionDataStore.findById(ref.ref))
      .flatMapConcat(refOpt => Source(refOpt.toList))
      .filter(_.enabled)
      .filter(_.subscriptionKind == ApiConsumerKind.Apikey)
      .filter(_.ownerRef == ctx.user.get.email)
      .flatMapConcat(sub => Source(sub.tokenRefs.map(r => (sub, r)).toList))
      .mapAsync(1) {
        case (sub, token) => env.datastores.apiKeyDataStore.findById(token).map(key => (sub, key))
      }
      .flatMapConcat {
        case (sub, opt) => Source(opt.map(key => (sub, key)).toList)
      }
      .runWith(Sink.seq)
  }
  def handleRedirections(doc: ApiDocumentation, path: String)(f: => Future[Result])(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    (doc.redirections :+ ApiDocumentationRedirection(Json.obj("from" -> "/logout", "to" -> "/.well-known/otoroshi/logout"))).find(_.from == path) match {
      case Some(redirection) => {
        Results.Redirect(redirection.to, 303).vfuture
      }
      case None => f
    }
  }
  def redirectToHome(api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    Results.Redirect(s"${config.prefix.getOrElse("")}/", 303).vfuture
  }
  def serveHome(api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    handleRedirections(doc, "/") {
      renderResource(doc.home).map {
        case (body, contentType) =>
          Results.Ok(baseTemplate(s"${api.name} - Portal", config.prefix.getOrElse(""), api, doc, ctx)(body.utf8String)).as("text/html")
      }
    }
  }
  def serveResource(api: Api, path: String, doc: ApiDocumentation, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    handleRedirections(doc, path) {
      val allResources = (doc.resources ++ Seq(doc.logo))
      val foundNavTop = doc.navigation.find(_.path.contains(path))
      allResources.find(_.path.contains(path)) match {
        case None if foundNavTop.isDefined && foundNavTop.get.items.nonEmpty => {
          val sidebar = foundNavTop.get
          val head = sidebar.items.flatMap {
            case link @ ApiDocumentationSidebarLink(_) => Seq(link)
            case category @ ApiDocumentationSidebarCategory(_) => category.links
          }.head
          allResources.find(_.path.contains(head.link)) match {
            case Some(resource) => {
              renderResource(resource).map {
                case (body, contentType) =>
                  val template = documentationPageTemplate(
                    s"${api.name} - ${resource.title.getOrElse("Documentation")}",
                    config.prefix.getOrElse(""),
                    api,
                    doc,
                    sidebar,
                    ctx
                  )(body.utf8String)
                  Results.Ok(template).as("text/html")
              }
            }
            case None => {
              Results.NotFound("Not found 1 !").vfuture
            }
          }
        }
        case None => Results.NotFound("Not found 2 !").vfuture
        case Some(resource) => renderResource(resource).map {
          case (body, contentType) =>
            if (resource.site_page) {
              val foundNavTop = doc.navigation.find(_.path.exists(str => path.startsWith(str)))
              val template = documentationPageTemplate(
                s"${api.name} - ${resource.title.getOrElse("Documentation")}",
                config.prefix.getOrElse(""),
                api,
                doc,
                foundNavTop.get,
                ctx
              )(body.utf8String)
              Results.Ok(template).as("text/html")
            } else {
              Results.Ok(body).as(contentType)
            }
        }
      }
    }
  }
  def serveApiDoc(api: Api, doc: ApiDocumentation, specPath: Option[String], ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    if (doc.references.nonEmpty && doc.references.size > 1) {
      val sidebar = ApiDocumentationSidebar(Json.obj(
        "items" -> JsArray(
          doc.references.map { r =>
            Json.obj(
              "label" -> r.title.json,
              "link" -> s"${config.prefix.getOrElse("")}/api-references${r.link}",
              "icon" -> r.icon.map(_.raw).getOrElse(Json.obj("text_content" -> "bi bi-braces")).asValue
            )
          }
        )
      ))
      specPath match {
        case None => {
          Results.Ok(documentationPageTemplate(s"${api.name} - API References", config.prefix.getOrElse(""), api, doc, sidebar, ctx)(
            s"""<div style="width: 100%; display: flex; flex-direction: row; justify-content: center;">
               |  <div class="container-xxl api-references-container" style="display: flex; flex-direction: row; flex-wrap: wrap;">
               |    ${doc.references.map(r => s"""
               |    |<a href="${config.prefix.getOrElse("")}/api-references${r.link}">
               |    |  <div class="card" style="width: 18rem; margin: 10px;">
               |    |    <div class="card-body">
               |    |      <h5 class="card-title">${r.title}</h5>
               |    |      <p class="card-text">${r.description.getOrElse("...")}</p>
               |    |    </div>
               |    |  </div>
               |    |</a>
               |    |""".stripMargin).mkString("\n")}
               |  </div>
               |</div>""".stripMargin
          )).as("text/html").vfuture
        }
        case Some(innerPath) => {
          doc.references.find(_.link.contains(innerPath)) match {
            case None => Results.NotFound("Not found inner !").vfuture
            case Some(ref) => {
              Results.Ok(documentationPageTemplate(s"${api.name} - API Reference", config.prefix.getOrElse(""), api, doc, sidebar, ctx, noInnerPadding = true)(
                s"""<div style="width: 100%; display: flex; flex-direction: row; justify-content: center;">
                   |  <div class="container-xxl redoc-container-div" style="display: flex; flex-direction: row; padding-right: 0px; padding-left: 0px;">
                   |    <redoc
                   |       spec-url="${config.prefix.getOrElse("")}${ref.link}"
                   |       hideHostname="false"
                   |       sanitize="true"
                   |       showObjectSchemaExamples="true"></redoc>
                   |  </div>
                   |  <!--script>
                   |    var interval = null;
                   |    interval = setInterval(function() {
                   |      console.log('interval');
                   |      var menu = document.querySelector('redoc .menu-content');
                   |      var error = document.querySelector('redoc h1');
                   |      if (menu) {
                   |        var node = document.createElement('select');
                   |        node.className = "form-control";
                   |        node.style.marginBottom = "10px";
                   |        node.innerHTML = '<option value="1.0.0">version 1.0.0</option><option value="2.0.0">version 2.0.0</option><option value="3.0.0">version 3.0.0</option>';
                   |        node.addEventListener('change', function (event) {
                   |          console.log('changed', node.value);
                   |          document.querySelector('redoc').setAttribute("spec-url", node.value + "-oas.json");
                   |        })
                   |        menu.prepend(node);
                   |        clearInterval(interval);
                   |      } else if (error && error.textContent.indexOf('Something went wrong...') > -1) {
                   |        console.log('found redoc error');
                   |        clearInterval(interval);
                   |        var node = document.createElement('select');
                   |        node.className = "form-control";
                   |        node.style.marginBottom = "10px";
                   |        node.innerHTML = '<option value="1.0.0">version 1.0.0</option><option value="2.0.0">version 2.0.0</option><option value="3.0.0">version 3.0.0</option>';
                   |        document.querySelector('.redoc-container-div').prepend(node);
                   |      }
                   |    }, 200);
                   |  </script-->
                   |</div>""".stripMargin
              )).as("text/html").vfuture
            }
          }
        }
      }
    } else if (doc.references.nonEmpty) {
      Results.Ok(baseTemplate(s"${api.name} - API Reference", config.prefix.getOrElse(""), api, doc, ctx)(
        s"""<div style="width: 100%; display: flex; flex-direction: row; justify-content: center;">
           |  <div class="container-xxl" style="display: flex; flex-direction: row;">
           |    <redoc
           |       spec-url="${config.prefix.getOrElse("")}${doc.references.headOption.map(_.link).getOrElse("/openapi.json")}"
           |       hideHostname="false"
           |       showObjectSchemaExamples="true"
           |    ></redoc>
           |  </div>
           |</div>""".stripMargin
      )).as("text/html").vfuture
    } else {
      Results.NotFound("Not found !").vfuture
    }
  }
  def serverJs(api: Api, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    Results.Ok("console.log('portal loaded !')").as("text/javascript").vfuture
  }
  def serveTesterPage(api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    Results.Ok(baseTemplate(s"${api.name} - Subscriptions", config.prefix.getOrElse(""), api, doc, ctx)("")).as("text/html").vfuture
  }
  def serveApikeysPage(api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    api.consumers.find(c => c.status == ApiConsumerStatus.Published) match {
      case None => Results.Ok(baseTemplate(s"${api.name} - Subscriptions", config.prefix.getOrElse(""), api, doc, ctx)("")).as("text/html").vfuture
      case Some(consumer) => apikeysFromApiForUser(consumer, ctx).flatMap { apikeys =>
        val sidebar = ApiDocumentationSidebar(Json.obj(
          "items" -> Json.arr(Json.obj(
            "label" -> "My apikeys",
            "link" -> s"${config.prefix.getOrElse("")}/subscriptions/apikeys",
            "icon" -> Json.obj("text_content" -> "bi bi-key")
          ))
        ))
        Results.Ok(documentationPageTemplate(s"${api.name} - Subscriptions", config.prefix.getOrElse(""), api, doc, sidebar, ctx)(
          s"""<div>
             |  <div style="width: 100%; display: flex; flex-direction: row; justify-content: flex-end;">
             |    <button type="button" class="btn btn-sm btn-outline-primary apikey-create" data-consumer="${consumer.id}"><span class="bi bi-plus-circle" /> apikey</button>
             |  </div>
             |  <table class="table">
             |    <thead>
             |      <tr>
             |        <th scope="col">#</th>
             |        <th scope="col">Name</th>
             |        <th scope="col">Enabled</th>
             |        <th scope="col">Actions</th>
             |      </tr>
             |    </thead>
             |    <tbody>
             |      ${apikeys.zipWithIndex.map { tuple =>
                      s"""<tr>
                         |  <th scope="row">${tuple._2}</th>
                         |  <td>${tuple._1._2.clientName}</td>
                         |  <td>${if (tuple._1._2.enabled) "<span class=\"badge rounded-pill text-bg-success\">yes</span>" else "<span class=\"badge rounded-pill text-bg-danger\">no</span>"}</td>
                         |  <td>
                         |    <div class="btn-group">
                         |      <button class="btn btn-sm btn-outline-success apikey-edit" title="edit apikey"
                         |        data-client-id="${tuple._1._2.clientId}"
                         |        data-sub="${tuple._1._1.id}"
                         |        data-consumer="${consumer.id}"
                         |      ><i class="bi bi-pencil-square"></i></button>
                         |      <button class="btn btn-sm btn-outline-primary apikey-bearer-copy" title="copy bearer"
                         |        data-bearer="${tuple._1._2.toBearer()}"
                         |      ><i class="bi bi-copy"></i></button>
                         |      <button class="btn btn-sm btn-outline-danger apikey-delete" title="delete apikey"
                         |        data-client-id="${tuple._1._2.clientId}"
                         |        data-sub="${tuple._1._1.id}"
                         |        data-consumer="${consumer.id}"
                         |      ><i class="bi bi-trash"></i></button></div>
                         |  </td>
                         |</tr>""".stripMargin
                    }.mkString("\n")}
             |    </tbody>
             |    <script>
             |      [].slice.call(document.querySelectorAll('[data-bearer]')).map(b => {
             |        b.addEventListener('click', () => {
             |          navigator.clipboard.writeText(b.getAttribute('data-bearer'))
             |        });
             |      });
             |      [].slice.call(document.querySelectorAll('.apikey-edit')).map(b => {
             |        b.addEventListener('click', () => {
             |          const client_id = b.getAttribute('data-client-id');
             |          const sub = b.getAttribute('data-sub');
             |          const consumer = b.getAttribute('data-consumer');
             |          let name = prompt("Apikey name ?");
             |          fetch('${config.prefix.getOrElse("")}/api/apikeys/' + client_id, {
             |            method: "PUT",
             |            credentials: "include",
             |            headers: {
             |              "Content-Type": "application/json",
             |            },
             |            body: JSON.stringify({
             |              sub, consumer, name
             |            })
             |          }).then(() => {
             |            setTimeout(() => {
             |              window.location.reload()
             |            }, 200);
           |            });
             |        });
             |      });
             |      [].slice.call(document.querySelectorAll('.apikey-create')).map(b => {
             |        b.addEventListener('click', () => {
             |          const consumer = b.getAttribute('data-consumer');
             |          let name = prompt("Apikey name ?");
             |          fetch('${config.prefix.getOrElse("")}/api/apikeys', {
             |            method: "POST",
             |            credentials: "include",
             |            headers: {
             |              "Content-Type": "application/json",
             |            },
             |            body: JSON.stringify({
             |              consumer, name
             |            })
             |          }).then(() => {
             |            setTimeout(() => {
             |              window.location.reload()
             |            }, 200);
           |            });
             |        });
             |      });
             |      [].slice.call(document.querySelectorAll('.apikey-delete')).map(b => {
             |        b.addEventListener('click', () => {
             |          const client_id = b.getAttribute('data-client-id');
             |          const sub = b.getAttribute('data-sub');
             |          const consumer = b.getAttribute('data-consumer');
             |          let ok = window.confirm("Are you sure you want to delete this apikey ?");
             |          if (ok) {
               |          fetch('${config.prefix.getOrElse("")}/api/apikeys/' + client_id, {
               |            method: "DELETE",
               |            credentials: "include",
               |          }).then(() => {
               |            setTimeout(() => {
               |              window.location.reload()
               |            }, 200);
             |            });
             |          }
             |        });
             |      });
             |    </script>
             |  </table>
             |</div>
             |""".stripMargin)).as("text/html").vfuture
      }
    }
  }
  def serveAllApikeys(api: Api, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    api.consumers.find(c => c.status == ApiConsumerStatus.Published) match {
      case None => Results.Ok(Json.arr()).vfuture
      case Some(consumer) => apikeysFromApiForUser(consumer, ctx).flatMap { apikeys =>
        Results.Ok(JsArray(apikeys.map {
          case (sub, key) =>
            Json.obj(
              "client_id" -> key.clientId,
              "name" -> key.clientName,
              "description" -> key.description,
              "bearer" -> key.toBearer(),
              "enabled" -> key.enabled,
              "sub" -> sub.id,
              "consumer" -> consumer.id,
            )
        })).as("application/json").vfuture
      }
    }
  }
  def serveApiTester(api: Api, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    ctx.request.body.runFold(ByteString.empty)(_ ++ _).flatMap { body =>
      val bodyJson = Json.parse(body.utf8String)
      val method = bodyJson.select("method").asOptString.getOrElse("GET")
      val url = bodyJson.select("url").asString
      val headers = bodyJson.select("headers").asOpt[Map[String, String]].getOrElse(Map.empty)
      val reqBody = bodyJson.select("body_json").asOpt[JsValue].filterNot(_ == JsNull)
      env.Ws.url(url).withFollowRedirects(false).withRequestTimeout(30.seconds).withMethod(method.toUpperCase()).withHttpHeaders(headers.toSeq: _*).applyOnWithOpt(reqBody) {
        case (req, body) => req.withBody(body)
      }.execute().map { resp =>
        Results.Ok(Json.obj(
          "status" -> resp.status,
          "headers" -> resp.headers.mapValues(_.last),
          "body_str" -> resp.body[String]
        ))
      }
    }
  }
  def serveCreateApikey(api: Api, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    ctx.request.body.runFold(ByteString.empty)(_ ++ _).flatMap { body =>
      val bodyJson = Json.parse(body.utf8String)
      println(bodyJson.prettify)
      val consumer_id = bodyJson.select("consumer").as[String]
      val nameOpt = bodyJson.select("name").asOpt[String]
      val descriptionOpt = bodyJson.select("description").asOpt[String]
      val enabledOpt = bodyJson.select("enabled").asOpt[Boolean]
      val planOpt = bodyJson.select("plan").asOpt[String]
      val clientName = nameOpt.getOrElse("New apikey")

      (for {
        user <- ctx.user
        consumer <- api.consumers.filter(c => c.status == ApiConsumerStatus.Published).find(_.id == consumer_id) if /*consumer.autoValidation && */consumer.consumerKind == ApiConsumerKind.Apikey // TODO: remove comment
        doc <- api.documentation
        plan <- doc.plans.find(p => planOpt.contains(p.id)).orElse(doc.plans.headOption)
      } yield {
        val sub_id = s"api-consumer-subscription_${IdGenerator.uuid}"
        val clientId = IdGenerator.lowerCaseToken(16)
        val sub = ApiConsumerSubscription(
          location = api.location,
          id = sub_id,
          name = s"subscription - ${clientName}",
          description = "",
          enabled = true,
          dates = ApiConsumerSubscriptionDates(
            created_at = DateTime.now(),
            processed_at = DateTime.now(),
            started_at = DateTime.now(),
            paused_at = DateTime.now(),
            ending_at = DateTime.now(),
            closed_at = DateTime.now(),
          ),
          ownerRef = user.email,
          consumerRef = consumer.id,
          apiRef = api.id,
          subscriptionKind = consumer.consumerKind,
          tokenRefs = Seq(clientId), // ref to apikey, cert, etc
          tags = Seq.empty,
          metadata = Map(
            "created_by" -> "otoroshi-api-portal-plugin",
            "otoroshi-api-plan-ref" -> plan.id,
            "otoroshi-api-plan-name" -> plan.name,
            "created_at" -> DateTime.now().toString(),
            "updated_at" -> DateTime.now().toString(),
          )
        )
        val newApikey = ApiKey(
          clientId = clientId,
          clientSecret = IdGenerator.lowerCaseToken(64),
          clientName = clientName,
          description = descriptionOpt.getOrElse(""),
          authorizedEntities = Seq(ApiIdentifier(api.id)),
          enabled = enabledOpt.getOrElse(true),
          throttlingQuota = plan.throttlingQuota,
          dailyQuota = plan.dailyQuota,
          monthlyQuota = plan.monthlyQuota,
          tags = plan.tags,
          metadata = plan.metadata ++ Map(
            "created_by" -> "otoroshi-api-portal-plugin",
            "otoroshi-api-ref" -> api.id,
            "otoroshi-api-sub-ref" -> sub.id,
            "otoroshi-api-consumer-ref" -> consumer.id,
            "otoroshi-api-plan-ref" -> plan.id,
            "otoroshi-api-plan-name" -> plan.name,
            "created_at" -> DateTime.now().toString(),
            "updated_at" -> DateTime.now().toString(),
          ),
          location = api.location,
        )
        if (env.clusterConfig.mode.isWorker) {
          ClusterAgent.clusterSaveApikey(env, newApikey)(ec, env.otoroshiMaterializer)
          // TODO: implement it
          // ClusterAgent.clusterSaveSub(env, sub)(ec, env.otoroshiMaterializer)
          // ClusterAgent.clusterSaveApi(env, sub)(ec, env.otoroshiMaterializer)
        }
        env.datastores.apiDataStore.set(api.copy(consumers = api.consumers.map { c =>
          if (c.id == consumer.id) {
            c.copy(
              subscriptions = c.subscriptions :+ ApiConsumerSubscriptionRef(sub_id),
            )
          } else {
            c
          }
        })).flatMap { _ =>
          env.datastores.apiConsumerSubscriptionDataStore.set(sub).flatMap { _ =>
            env.datastores.apiKeyDataStore.set(newApikey).map { _ =>
              Results.Ok(Json.obj(
                "client_id" -> newApikey.clientId,
                "name" -> newApikey.clientName,
                "description" -> newApikey.description,
                "bearer" -> newApikey.toBearer(),
                "enabled" -> newApikey.enabled,
                "sub" -> sub.id,
                "consumer" -> consumer.id,
              )).as("application/json")
            }
          }
        }
      }).getOrElse {
        Results.BadRequest(Json.obj("error" -> "bad request")).vfuture
      }
    }
  }
  def serveDeleteApikey(api: Api, client_id: String, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    ctx.user match {
      case None => Results.Unauthorized(Json.obj("error" -> "user not found")).vfuture
      case Some(user) => env.datastores.apiKeyDataStore.findById(client_id).flatMap {
        case None => Results.Unauthorized(Json.obj("error" -> "apikey not found")).vfuture
        case Some(apikey) => {
          apikey.metadata.get("otoroshi-api-consumer-ref") match {
            case None => Results.Unauthorized(Json.obj("error" -> "cref not found")).vfuture
            case Some(consumer_id) => apikey.metadata.get("otoroshi-api-sub-ref") match {
              case None => Results.Unauthorized(Json.obj("error" -> "sref not found")).vfuture
              case Some(sub_id) => api.consumers.filter(c => c.status == ApiConsumerStatus.Published).find(_.id == consumer_id) match {
                case None => Results.Unauthorized(Json.obj("error" -> "consumer not found")).vfuture
                case Some(consumer) => consumer.subscriptions.find(_.ref == sub_id) match {
                  case None => Results.Unauthorized(Json.obj("error" -> "subref not found")).vfuture
                  case Some(subRef) => env.datastores.apiConsumerSubscriptionDataStore.findById(subRef.ref).flatMap {
                    case Some(sub) if sub.ownerRef == user.email => {
                      if (env.clusterConfig.mode.isWorker) {
                        ClusterAgent.clusterDeleteApikey(env, apikey.clientId)(ec, env.otoroshiMaterializer)
                        // TODO: implement it
                        // ClusterAgent.clusterDeleteSub(env, sub)(ec, env.otoroshiMaterializer)
                        // ClusterAgent.clusterDeleteApi(env, sub)(ec, env.otoroshiMaterializer)
                      }
                      env.datastores.apiDataStore.set(api.copy(consumers = api.consumers.map { c =>
                        if (c.id == consumer.id) {
                          c.copy(
                            subscriptions = c.subscriptions.filterNot(_.ref == sub_id),
                          )
                        } else {
                          c
                        }
                      })).flatMap { _ =>
                        env.datastores.apiConsumerSubscriptionDataStore.delete(sub.id).flatMap { _ =>
                          env.datastores.apiKeyDataStore.delete(apikey.clientId).map { _ =>
                            Results.NoContent
                          }
                        }
                      }
                    }
                    case None => Results.Unauthorized(Json.obj("error" -> "sub not found")).vfuture
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  def serveUpdateApikey(api: Api, client_id: String, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    ctx.request.body.runFold(ByteString.empty)(_ ++ _).flatMap { body =>
      val bodyJson = Json.parse(body.utf8String)
      println(bodyJson.prettify)
      val nameOpt = bodyJson.select("name").asOpt[String]
      val descriptionOpt = bodyJson.select("description").asOpt[String]
      val enabledOpt = bodyJson.select("enabled").asOpt[Boolean]
      ctx.user match {
        case None => Results.Unauthorized(Json.obj("error" -> "user not found")).vfuture
        case Some(user) => env.datastores.apiKeyDataStore.findById(client_id).flatMap {
          case None => Results.Unauthorized(Json.obj("error" -> "apikey not found")).vfuture
          case Some(apikey) => {
            apikey.metadata.get("otoroshi-api-consumer-ref") match {
              case None => Results.Unauthorized(Json.obj("error" -> "cref not found")).vfuture
              case Some(consumer_id) => apikey.metadata.get("otoroshi-api-sub-ref") match {
                case None => Results.Unauthorized(Json.obj("error" -> "sref not found")).vfuture
                case Some(sub_id) => api.consumers.filter(c => c.status == ApiConsumerStatus.Published).find(_.id == consumer_id) match {
                  case None => Results.Unauthorized(Json.obj("error" -> "consumer not found")).vfuture
                  case Some(consumer) => consumer.subscriptions.find(_.ref == sub_id) match {
                    case None => Results.Unauthorized(Json.obj("error" -> "subref not found")).vfuture
                    case Some(subRef) => env.datastores.apiConsumerSubscriptionDataStore.findById(subRef.ref).flatMap {
                      case Some(sub) if sub.ownerRef == user.email => {
                        val newApikey = apikey.copy(
                          clientName = nameOpt.getOrElse(apikey.clientName),
                          description = descriptionOpt.getOrElse(apikey.description),
                          enabled = enabledOpt.getOrElse(apikey.enabled),
                        )
                        if (env.clusterConfig.mode.isWorker) {
                          ClusterAgent.clusterSaveApikey(env, newApikey)(ec, env.otoroshiMaterializer)
                          // TODO
                          // ClusterAgent.clusterSaveSub(env, sub)(ec, env.otoroshiMaterializer)
                        }
                        env.datastores.apiConsumerSubscriptionDataStore.set(sub.copy(name = s"subscription - ${newApikey.clientName}")).flatMap { _ =>
                          env.datastores.apiKeyDataStore.set(newApikey).map { _ =>
                            Results.Ok(Json.obj(
                              "client_id" -> newApikey.clientId,
                              "name" -> newApikey.clientName,
                              "description" -> newApikey.description,
                              "bearer" -> newApikey.toBearer(),
                              "enabled" -> newApikey.enabled,
                              "sub" -> sub.id,
                              "consumer" -> consumer.id,
                            )).as("application/json")
                          }
                        }
                      }
                      case None => Results.Unauthorized(Json.obj("error" -> "sub not found")).vfuture
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def renderResourceAsIcon(resource: Option[ApiDocumentationResource], style: Option[String] = None): String = {
    resource match {
      case Some(resource) if resource.text_content.isDefined => s"""<i class="${resource.text_content.get}"></i> """
      case Some(resource) if resource.url.isDefined => s"""<img src="${resource.url.get}" style="${resource.raw.select("style").asOptString.orElse(style).getOrElse("")}" /> """
      case Some(resource) if resource.base64_content.isDefined => s"""<img src="data:${resource.contentType};base64,${resource.base64_content.get}" style="${resource.raw.select("style").asOptString.orElse(style).getOrElse("")}" /> """
      case _ => ""
    }
  }

  def renderResource(resource: ApiDocumentationResource)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[(ByteString, String)] = {
    def handleTransform(byteString: ByteString): ByteString = {
      // TODO: handle EL
      if (resource.transform.contains("markdown")) {
        ByteString(s"""<zero-md><script type="text/markdown">${byteString.utf8String}</script></zero-md>""".stripMargin)
      } else if (resource.transform.contains("redoc")) {
        ByteString(s"""<redoc
                      |   spec-url="${resource.url.getOrElse("#")}"
                      |   hideHostname="false"
                      |   showObjectSchemaExamples="true"
                      |></redoc>""".stripMargin)
      } else {
        byteString
      }
    }
    resource.url match {
      case Some(url) => env.Ws.url(url).withFollowRedirects(true).withRequestTimeout(20.seconds).get() map { resp =>
        //if (url == "https://rickandmorty.zuplo.io/openapi.json") {
        //  ((resp.json.asObject ++ Json.obj(
        //    "servers" -> Json.arr(
        //      Json.obj(
        //        "url" -> "https://rickandmorty.zuplo.io/dev/v1",
        //        "description" -> "The dev API endpoint",
        //      ),
        //      Json.obj(
        //        "url" -> "https://rickandmorty.zuplo.io/staging/v1",
        //        "description" -> "The staging API endpoint",
        //      ),
        //      Json.obj(
        //        "url" -> "https://rickandmorty.zuplo.io/v1",
        //        "description" -> "The prod API endpoint",
        //      )
        //    )
        //  )).prettify.byteString, resource.contentType)
        //} else {
          (handleTransform(resp.bodyAsBytes), resource.contentType)
        //}
      } // TODO: add more options
      case None => {
        resource.base64_content match {
          case Some(bs) => (bs, resource.contentType).vfuture
          case None => resource.json_content match {
            case Some(json) => (json.stringify.byteString, resource.contentType).vfuture
            case None => resource.text_content match {
              case Some(text) => (handleTransform(ByteString(text)), resource.contentType).vfuture
              case None => (ByteString("no content"), resource.contentType).vfuture
            }
          }
        }
      }
    }
  }

  def documentationPageTemplate(title: String, prefix: String, api: Api, doc: ApiDocumentation, sidebar: ApiDocumentationSidebar, ctx: NgbBackendCallContext, noInnerPadding: Boolean = false)(content: String)(implicit  env: Env, ec: ExecutionContext, mat: Materializer) = {
    val sidebarHtml = s"""<aside class="col-lg-2" style="width: 20%;">
           |  <div class="sidebar pe-lg-3">
           |    <div class="" style="margin-top: 1.5rem;"></div>
           |    <div class="mb-3 d-lg-none">
           |      <button class="btn btn-outline-secondary w-100" data-bs-toggle="collapse" data-bs-target="#docSidebar">
           |        Menu
           |      </button>
           |    </div>
           |    <nav id="docSidebar" class="collapse d-lg-block">
           |      <div class="nav flex-column gap-2">
           |        ${sidebarTemplate(sidebar, prefix, ctx)}
           |      </div>
           |    </nav>
           |  </div>
           |</aside>""".stripMargin
    baseTemplate(title, prefix, api, doc, ctx)(
      s"""<div style="width: 100%; display: flex; flex-direction: row; justify-content: center;">
         |  <div class="container-xxl" style="display: flex; flex-direction: row;">
         |    ${sidebarHtml}
         |    <div style="width:100%; ${if (noInnerPadding) "" else "padding:1.5rem;"}">
         |      ${content}
         |    </div>
         |  </div>
         |</div>
         |""".stripMargin)
  }

  def sidebarCategoryTemplate(item: ApiDocumentationSidebarCategory, prefix: String, index: Int, ctx: NgbBackendCallContext): String = {
    s"""<div class="nav-section sidebar-section">
       |  <button class="btn btn-toggle w-100 text-start" data-bs-toggle="collapse" data-bs-target="#section-${index}">
       |    <i class="bi bi-chevron-right"></i> ${renderResourceAsIcon(item.icon)}${item.label}
       |  </button>
       |  <div id="section-${index}" class="collapse show ps-3 mt-2">
       |    ${item.links.map(l => sidebarLinkTemplate(l, prefix, ctx)).mkString("\n")}
       |  </div>
       |</div>""".stripMargin
  }

  def sidebarLinkTemplate(item: ApiDocumentationSidebarLink, prefix: String, ctx: NgbBackendCallContext): String = {
    s"""<a class="nav-link ${if(ctx.request.path == s"${prefix}${item.link}") "active" else ""}" style="font-weight: ${if(ctx.request.path == s"${prefix}${item.link}") "600" else "normal"}" href="${prefix}${item.link}">${renderResourceAsIcon(item.icon)}${item.label}</a>""".stripMargin
  }

  def sidebarTemplate(sidebar: ApiDocumentationSidebar, prefix: String, ctx: NgbBackendCallContext): String = {
    sidebar.items.zipWithIndex.map {
      case (item @ ApiDocumentationSidebarCategory(_), idx) => sidebarCategoryTemplate(item, prefix, idx, ctx)
      case (item @ ApiDocumentationSidebarLink(_), idx) => sidebarLinkTemplate(item, prefix, ctx)
    }.mkString("\n")
  }

  def navPathActive(paths: Seq[String], ctx: NgbBackendCallContext, prefix: String): String = {
    val path = ctx.request.path.replaceFirst(prefix, "").toLowerCase()
    if (paths.exists(p => path.startsWith(p))) {
      "active fw-medium"
    } else {
      ""
    }
  }

  def baseTemplate(title: String, prefix: String, api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext)(content: String): String = {
    s"""
       |<!doctype html>
       |<html lang="en" data-bs-theme="light">
       |  <head>
       |    <meta charset="utf-8" />
       |    <meta name="viewport" content="width=device-width, initial-scale=1" />
       |    <title>${title}</title>
       |    <link rel="icon" href="${prefix}${doc.logo.url.getOrElse("#")}">
       |    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB" crossorigin="anonymous">
       |    <link  href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.css" rel="stylesheet" type="text/css" />
       |    <style>
       |      .sidebar .nav-link {
       |        color: inherit !important;
       |        text-decoration: none !important;
       |      }
       |      .sidebar .nav-link:visited,
       |      .sidebar .nav-link:hover,
       |      .sidebar .nav-link:active,
       |      .sidebar .nav-link:focus {
       |        color: inherit;
       |        text-decoration: none;
       |      }
       |      .sidebar .nav-link .active {
       |        font-weight: 600 !important;
       |      }
       |
       |      .api-references-container a {
       |        color: inherit !important;
       |        text-decoration: none !important;
       |      }
       |
       |      .api-references-container a:visited,
       |      .api-references-container a:hover,
       |      .api-references-container a:active,
       |      .api-references-container a:focus {
       |        color: inherit;
       |        text-decoration: none;
       |      }
       |      .sidebar-section button {
       |        border-width: 1px;
       |        border-color: var(--bs-secondary-color);
       |      }
       |    </style>
       |
       |    <style>
       |      /* Hero */
       |      .hero-badge {
       |        background: #f2d14b;
       |        border-radius: .75rem;
       |        font-weight: 800;
       |        display: inline-block;
       |        padding: .1rem .5rem;
       |        line-height: 1.2;
       |      }
       |      .hero-title {
       |        letter-spacing: -0.02em;
       |        font-weight: 800;
       |      }
       |      .hero-subtle {
       |        color: var(--bs-secondary-color);
       |      }
       |      /* Right card image placeholder keeps ratio */
       |      .illustration-card {
       |        background: #f2d14b;
       |        border-radius: 1rem;
       |        box-shadow: var(--bs-box-shadow-lg);
       |      }
       |      .illustration-card .inner {
       |        min-height: 440px;
       |        background:
       |          radial-gradient(120px 120px at 75% 55%, rgba(255,255,255,.25), transparent 60%),
       |          radial-gradient(120px 120px at 35% 65%, rgba(0,0,0,.05), transparent 60%);
       |        display:flex;align-items:center;justify-content:center;
       |        font-size: clamp(1.25rem, 2vw, 2rem);
       |        font-weight: 800;
       |      }
       |      /* Footer subtle links */
       |      .footer-links a { color: var(--bs-secondary-color); text-decoration: none; }
       |      .footer-links a:hover { text-decoration: underline; }
       |      /* Make navbar sticky and add subtle border */
       |      .navbar.is-sticky { position: sticky; top: 0; z-index: 1030; border-bottom: 1px solid var(--bs-border-color); background: var(--bs-body-bg); }
       |      /* Search input look like your screenshot */
       |      .nav-search { max-width: 360px; }
       |      .navbar.is-sticky { position: sticky; top: 0; z-index: 1030; border-bottom: 1px solid var(--bs-border-color); background: var(--bs-body-bg); }
       |      .sidebar {
       |        position: sticky;
       |        top: 4.25rem; /* below navbar */
       |        height: calc(100vh - 4rem);
       |        max-height: calc(100dvh - 4rem);
       |        overflow-y: auto;
       |        border-right: 1px solid var(--bs-border-color);
       |      }
       |      @media (max-width: 991.98px) {
       |        .sidebar { position: static; max-height: none; border-right: 0; }
       |      }
       |      .toc {
       |        position: sticky;
       |        top: 4.25rem;
       |        max-height: calc(100dvh - 5rem);
       |        overflow-y: auto;
       |      }
       |      .doc-hero {
       |        background: #f2d14b;
       |        border-radius: 1rem;
       |        height: 220px;
       |        display:flex;align-items:center;justify-content:center;
       |        font-weight: 800;
       |        font-size: clamp(1.2rem, 2vw, 1.75rem);
       |        margin-bottom: 1.5rem;
       |      }
       |      .small-muted { color: var(--bs-secondary-color); }
       |      .footer-links a { color: var(--bs-secondary-color); text-decoration: none; }
       |      .footer-links a:hover { text-decoration: underline; }
       |      .nav-section .btn-toggle { font-weight: 600; }
       |      .nav-section .btn-toggle::after { content: '\f285'; font-family: bootstrap-icons; transition:.2s; margin-left: .25rem; }
       |      .nav-section .btn-toggle[aria-expanded="true"]::after { transform: rotate(90deg); }
       |      .toc a { text-decoration: none; }
       |      .sidebar .nav-link {
       |        color: black !important;
       |        text-decoration: none !important;
       |      }
       |      .sidebar .nav-link:visited,
       |      .sidebar .nav-link:hover,
       |      .sidebar .nav-link:active,
       |      .sidebar .nav-link:focus {
       |        color: black;
       |        text-decoration: none;
       |      }
       |    </style>
       |  </head>
       |  <body data-barba="wrapper">
       |    <main data-barba="container" data-barba-namespace="main">
       |    <!-- Top nav -->
       |    <nav class="navbar is-sticky navbar-expand-lg" style="height: 120px; flex-direction: column;">
       |      <div class="container-xxl">
       |        <a class="navbar-brand fw-bold" href="${prefix}/">
       |          ${renderResourceAsIcon(doc.logo.some, Some("max-height: 45px;"))}${api.name}
       |        </a>
       |        <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#mainNav">
       |          <span class="navbar-toggler-icon"></span>
       |        </button>
       |        <div class="collapse navbar-collapse" id="mainNav">
       |          <form class="d-none d-lg-flex ms-auto me-3 nav-search" role="search">
       |            <div class="input-group">
       |              <span class="input-group-text bg-transparent border-end-0"><i class="bi bi-search"></i></span>
       |              <input type="search" class="form-control border-start-0" style="border-bottom-right-radius: 6px; border-top-right-radius: 6px;" placeholder="Search" aria-label="Search" />
       |            </div>
       |          </form>
       |          <div class="d-flex align-items-center gap-2">
       |            <button id="themeToggle" class="btn btn-outline-secondary" type="button" aria-label="Toggle theme">
       |              <i class="bi bi-sun"></i>
       |            </button>
       |            ${if(ctx.user.isDefined) {
                      s"""<div class="dropdown" style="z-index: 9999;">
                         |   <button class="btn btn-outline-secondary dropdown-toggle" type="button" id="identity-button" data-bs-toggle="dropdown">
                         |     <i class="bi bi-person-badge"></i>
                         |   </button>
                         |   <div class="dropdown-menu" style="z-index: 9999;">
                         |     <a class="dropdown-item disabled" aria-disabled="true" href="#" tabindex="-1">${ctx.user.get.name}</a>
                         |     <a class="dropdown-item disabled" aria-disabled="true" href="#" tabindex="-1">${ctx.user.get.email}</a>
                         |     <a class="dropdown-item" href="${prefix}/logout">Logout</a>
                         |   </div>
                         |</div>
                         |""".stripMargin
                    } else {
                      s"""<a class="btn btn-outline-primary" href="${prefix}/login?redirect=${prefix}/">Login</a>"""
                    }}
       |          </div>
       |        </div>
       |      </div>
       |      <div style="width: 100vw; border-top: 1px solid var(--bs-border-color); height: 5px;"></div>
       |      <div class="container-xxl" style="">
       |        <ul class="navbar-nav me-3">
       |          ${doc.navigation.map(nav => s"""<li class="nav-item"><a class="nav-link ${navPathActive(nav.path, ctx, prefix)}" href="${prefix}${nav.path.headOption.getOrElse("#")}">${renderResourceAsIcon(nav.icon, Some("max-height: 35px"))}${nav.label}</a></li>""").mkString("\n")}
       |          <li class="nav-item"><a class="nav-link ${navPathActive(Seq("/api-references"), ctx, prefix)}" href="${prefix}/api-references">API Reference</a></li>
       |          ${if (ctx.user.isDefined && api.consumers.exists(_.consumerKind != ApiConsumerKind.Keyless)) {
                    s"""<li class="nav-item"><a class="nav-link ${navPathActive(Seq("/subscriptions"), ctx, prefix)}" href="${prefix}/subscriptions">Subscriptions</a></li>"""
                  } else ""}
       |          <li class="nav-item"><a class="nav-link ${navPathActive(Seq("/tester"), ctx, prefix)}" href="${prefix}/tester">Tester</a></li>
       |        </ul>
       |      </div>
       |    </nav>
       |    ${content}
       |    </main>
       |    <!-- JS -->
       |    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/js/bootstrap.bundle.min.js" integrity="sha384-FKyoEForCGlyvwx9Hj09JcYn3nv7wiPVlz7YYwJrWVcXK/BmnVDxM+D2scQbITxI" crossorigin="anonymous"></script>
       |    <script src="https://cdn.redoc.ly/redoc/latest/bundles/redoc.standalone.js"></script>
       |    <script type="text/javascript" src="${prefix}/portal.js"></script>
       |    <script>
       |      // Theme toggle with persistence
       |      (function() {
       |        const key = 'portal-theme';
       |        const root = document.documentElement;
       |        const btn  = document.getElementById('themeToggle');
       |        function setIcon(theme) {
       |          btn.innerHTML = theme === 'dark'
       |            ? '<i class="bi bi-moon-stars"></i>'
       |            : '<i class="bi bi-sun"></i>';
       |        }
       |        function apply(theme) {
       |          root.setAttribute('data-bs-theme', theme);
       |          localStorage.setItem(key, theme);
       |          setIcon(theme);
       |        }
       |        const preferred = localStorage.getItem(key) || (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
       |        apply(preferred);
       |        btn.addEventListener('click', () => {
       |          apply(root.getAttribute('data-bs-theme') === 'dark' ? 'light' : 'dark');
       |        });
       |      })();
       |    </script>
       |    <script type="module" src="https://cdn.jsdelivr.net/npm/zero-md@3?register"></script>
       |    <script src="https://cdn.jsdelivr.net/npm/@barba/core"></script>
       |    <script>
       |      // barba.init({
       |      //   // ...
       |      // })
       |    </script>
       |  </body>
       |</html>
       |""".stripMargin
  }

}