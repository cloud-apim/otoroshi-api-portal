package otoroshi_plugins.com.cloud.apim.plugins.apiportal

import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import next.models._
import org.joda.time.DateTime
import otoroshi.cluster.ClusterAgent
import otoroshi.env.Env
import otoroshi.models.{ApiIdentifier, ApiKey, RemainingQuotas}
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
        case Some(api) => api
          .applyOnIf(api.metadata.get("doc_template").contains("wines"))(_.copy(documentation = ApiDocumentationExample.wines.some))
          .applyOnIf(api.metadata.get("doc_template").contains("remote"))(_.copy(documentation = ApiDocumentationExample.remote.some))
          .applyOnIf(api.metadata.get("doc_template").contains("remote_test"))(_.copy(documentation = ApiDocumentationExample.remoteTest.some))
          .applyOnIf(api.metadata.get("doc_template").contains("remote_wines"))(_.copy(documentation = ApiDocumentationExample.remoteWines.some))
          .applyOnIf(api.metadata.get("doc_template").contains("test"))(_.copy(documentation = ApiDocumentationExample.value.some))
          .resolveDocumentation() flatMap {
            case Some(doc) if doc.enabled => {
              val prefix = config.prefix.getOrElse("")
              val path = ctx.request.path.replaceFirst(prefix, "").toLowerCase()
              (ctx.request.method.toLowerCase(), path) match {
                case ("put", path) if path.startsWith("/api/apikeys/") => OtoroshiApiPortal.serveUpdateApikey(api, path.replaceFirst("/api/apikeys/", ""), ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
                case ("delete", path) if path.startsWith("/api/apikeys/") => OtoroshiApiPortal.serveDeleteApikey(api, path.replaceFirst("/api/apikeys/", ""), ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
                case ("post", "/api/apikeys") => OtoroshiApiPortal.serveCreateApikey(api, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
                case ("get",  "/api/apikeys") => OtoroshiApiPortal.serveAllApikeys(api, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
                case ("post", "/api/_test") => OtoroshiApiPortal.serveApiTester(api, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
                case ("get", "/api/plans") => OtoroshiApiPortal.servePlansJson(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
                case ("get", "/api/documentation") => OtoroshiApiPortal.serveDocumentationJson(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
                /////////////////////
                case ("get", "/subscriptions/apikeys") => OtoroshiApiPortal.serveApikeysPage(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
                case ("get", "/subscriptions") => OtoroshiApiPortal.serveApikeysPage(api, doc, ctx, config).map(r => BackendCallResponse(NgPluginHttpResponse.fromResult(r), None).right)
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
  def apikeysFromApiForUser(plan: ApiDocumentationPlan, ctx: NgbBackendCallContext)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Seq[(ApiSubscription, ApiKey)]] = {
    Source(env.proxyState.allApiSubscriptions().toList)
      .filter(_.planRef == plan.id)
      .filter(_.status == ApiSubscriptionEnabled)
      .filter(_.subscriptionKind == ApiKind.Apikey)
      .filter(c => ctx.user.map(_.email).contains(c.ownerRef))
      .flatMapConcat(sub => if (sub.tokenRefs.isEmpty) Source.single((sub, "--")) else Source(sub.tokenRefs.filter(_.select("apikey").isDefined).map(r => (sub, r.select("apikey").asString)).toList))
      .mapAsync(1) {
        case (sub, tok) if tok == "--" && sub.tokenRefs.isEmpty => (sub, ApiKey(
          clientId = IdGenerator.token(16),
          clientSecret = "--",
          clientName = sub.metadata.getOrElse("otoroshi-api-client-name", "--"),
          description = sub.description,
          authorizedEntities = Seq.empty,
          enabled = false,
          throttlingQuota = 100L,
          dailyQuota = 100L,
          monthlyQuota = 100L,
          tags = Seq.empty,
          metadata = Map(
            "waiting-approval" -> "true",
            "otoroshi-api-plan-ref" -> sub.metadata.getOrElse("otoroshi-api-plan-ref", "--"),
          ),
          location = sub.location,
        ).some).vfuture
        case (sub, token) =>
          env.datastores.apiKeyDataStore.findById(token).map(key => (sub, key))
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
      renderResource(doc.home, doc).map {
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
              renderResource(resource, doc).map {
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
        case Some(resource) => renderResource(resource, doc).map {
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
              "icon" -> r.icon.map(_.raw).getOrElse(Json.obj("css_icon_class" -> "bi bi-braces")).asValue
            )
          }
        )
      ))
      specPath match {
        case None => {
          Results.Ok(documentationPageTemplate(s"${api.name} - API References", config.prefix.getOrElse(""), api, doc, sidebar, ctx)(
            s"""<div class="space-y-6">
               |  <section class="rounded-[28px] border border-slate-200/80 bg-gradient-to-br from-white to-stone-100 p-7 shadow-sm dark:border-white/10 dark:from-slate-900 dark:to-slate-900/70">
               |    <span class="portal-eyebrow">Reference</span>
               |    <h1 class="portal-page-title">API references</h1>
               |    <p class="portal-page-subtitle">Pick a specification and explore operations, schemas and live examples.</p>
               |  </section>
               |  <div class="grid gap-4 md:grid-cols-2">
               |    ${doc.references.map(r => s"""
               |    |<a class="group rounded-[26px] border border-slate-200/80 bg-white/90 p-6 shadow-sm transition hover:-translate-y-1 hover:shadow-lg dark:border-white/10 dark:bg-slate-900/80" href="${config.prefix.getOrElse("")}/api-references${r.link}">
               |    |  <div class="flex items-start justify-between gap-4">
               |    |    <div class="space-y-3">
               |    |      <div class="inline-flex h-11 w-11 items-center justify-center rounded-2xl bg-sky-50 text-sky-700 dark:bg-sky-500/10 dark:text-sky-300">${renderResourceAsIcon(r.icon, doc)}</div>
               |    |      <div>
               |    |        <h2 class="text-xl font-semibold tracking-tight text-slate-950 dark:text-white">${r.title}</h2>
               |    |        <p class="mt-2 text-sm leading-6 text-slate-600 dark:text-slate-300">${r.description.getOrElse("Open the specification and inspect endpoints, examples and response shapes.")}</p>
               |    |      </div>
               |    |    </div>
               |    |    <span class="inline-flex items-center gap-2 rounded-full bg-stone-100 px-3 py-1 text-sm font-medium text-slate-600 transition group-hover:bg-slate-950 group-hover:text-white dark:bg-white/10 dark:text-slate-300 dark:group-hover:bg-white dark:group-hover:text-slate-950">Open <i class="bi bi-arrow-right"></i></span>
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
                s"""<div class="space-y-6 p-6 sm:p-8">
                   |  <section class="rounded-[28px] border border-slate-200/80 bg-gradient-to-br from-white to-stone-100 p-7 shadow-sm dark:border-white/10 dark:from-slate-900 dark:to-slate-900/70">
                   |    <span class="portal-eyebrow">Reference</span>
                   |    <h1 class="portal-page-title">${ref.title}</h1>
                   |    <p class="portal-page-subtitle">Read operations, inspect examples and launch the built-in tester directly from the reference.</p>
                   |  </section>
                   |  <div class="portal-redoc-shell overflow-hidden rounded-[28px] border border-slate-200/80 bg-white shadow-sm dark:border-white/10 dark:bg-slate-950">
                   |    <!--redoc
                   |       spec-url="${config.prefix.getOrElse("")}${ref.link}"
                   |       hideHostname="false"
                   |       sanitize="true"
                   |       showObjectSchemaExamples="true"></redoc-->
                   |     <div id="scalar-doc"></div>
                   |  </div>
                   |  <script>
                   |    var interval = null;
                   |    Scalar.createApiReference('#scalar-doc', {
                   |      // The URL of the OpenAPI/Swagger document
                   |      url: '${config.prefix.getOrElse("")}${ref.link}',
                   |      hideDarkModeToggle: true,
                   |      forceDarkModeState: localStorage.getItem('portal-theme-mode') === 'dark' ? 'dark' :  (localStorage.getItem('portal-theme-mode') === 'system' ? (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light') : 'light'),
                   |      // Avoid CORS issues
                   |      //proxyUrl: 'https://proxy.scalar.com',
                   |    })
                   |    interval = setInterval(function() {
                   |      var menu = document.querySelector('redoc .menu-content');
                   |      if (menu) {
                   |        [].slice.call(document.querySelectorAll('.sc-iGgWBj.sc-gsFSXq.lbpUdJ.bOFhJE')).map(function(node) {
                   |          if (node.querySelector('.portal-redoc-action')) { return; }
                   |          var button = document.createElement('button');
                   |          button.type = 'button';
                   |          button.className = 'portal-redoc-action';
                   |          button.innerHTML = '<span>Test Request</span>';
                   |          button.addEventListener('click', function() {
                   |            var verb = node.childNodes[1].childNodes[0].childNodes[0].getAttribute('type').toUpperCase();
                   |            var url = node.childNodes[1].childNodes[1].childNodes[0].childNodes[1].childNodes[0].textContent;
                   |            var body = null;
                   |            var presetHeaders = { 'Accept': 'application/json' };
                   |            if (node.childNodes.length === 4) {
                   |              body = node.childNodes[2].querySelector('code').textContent;
                   |              presetHeaders['Content-Type'] = 'application/json';
                   |            }
                   |            openApiTester({ method: verb, url: url, presetBody: body, presetHeaders: presetHeaders });
                   |          });
                   |          node.prepend(button);
                   |        });
                   |        clearInterval(interval);
                   |      }
                   |    }, 200);
                   |  </script>
                   |</div>""".stripMargin
              )).as("text/html").vfuture
            }
          }
        }
      }
    } else if (doc.references.nonEmpty) {
      Results.Ok(baseTemplate(s"${api.name} - API Reference", config.prefix.getOrElse(""), api, doc, ctx)(
        s"""<section class="pb-14 pt-8 lg:pt-10">
           |  <div class="container-xxl space-y-6">
           |    <section class="rounded-[28px] border border-slate-200/80 bg-gradient-to-br from-white to-stone-100 p-7 shadow-sm dark:border-white/10 dark:from-slate-900 dark:to-slate-900/70">
           |      <span class="portal-eyebrow">Reference</span>
           |      <h1 class="portal-page-title">${doc.references.head.title}</h1>
           |      <p class="portal-page-subtitle">Inspect request shapes, read examples and launch the tester from any operation.</p>
           |    </section>
           |    <div class="portal-redoc-shell overflow-hidden rounded-[28px] border border-slate-200/80 bg-white shadow-sm dark:border-white/10 dark:bg-slate-950">
           |      <!--redoc
           |         spec-url="${config.prefix.getOrElse("")}${doc.references.headOption.map(_.link).getOrElse("/openapi.json")}"
           |         hideHostname="false"
           |         showObjectSchemaExamples="true"
           |      ></redoc-->
           |      <div id="scalar-doc"></div>
           |      <script>
           |      Scalar.createApiReference('#scalar-doc', {
           |        // The URL of the OpenAPI/Swagger document
           |        url: '${config.prefix.getOrElse("")}${doc.references.headOption.map(_.link).getOrElse("/openapi.json")}',
           |        hideDarkModeToggle: true,
           |        forceDarkModeState: localStorage.getItem('portal-theme-mode') === 'dark' ? 'dark' :  (localStorage.getItem('portal-theme-mode') === 'system' ? (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light') : 'light'),
           |        // Avoid CORS issues
           |        //proxyUrl: 'https://proxy.scalar.com',
           |      })
           |        var interval = null;
           |        interval = setInterval(function() {
           |          var menu = document.querySelector('redoc .menu-content');
           |          if (menu) {
           |            [].slice.call(document.querySelectorAll('.sc-iGgWBj.sc-gsFSXq.lbpUdJ.bOFhJE')).map(function(node) {
           |              if (node.querySelector('.portal-redoc-action')) { return; }
           |              var button = document.createElement('button');
           |              button.type = 'button';
           |              button.className = 'portal-redoc-action';
           |              button.innerHTML = '<span>Test Request</span>';
           |              button.addEventListener('click', function() {
           |                var verb = node.childNodes[1].childNodes[0].childNodes[0].getAttribute('type').toUpperCase();
           |                var url = node.childNodes[1].childNodes[1].childNodes[0].childNodes[1].childNodes[0].textContent;
           |                var body = null;
           |                var presetHeaders = { 'Accept': 'application/json' };
           |                if (node.childNodes.length === 4) {
           |                  body = node.childNodes[2].querySelector('code').textContent;
           |                  presetHeaders['Content-Type'] = 'application/json';
           |                }
           |                openApiTester({ method: verb, url: url, presetBody: body, presetHeaders: presetHeaders });
           |              });
           |              node.prepend(button);
           |            });
           |            clearInterval(interval);
           |          }
           |        }, 200);
           |      </script>
           |    </div>
           |  </div>
           |</section>""".stripMargin
      )).as("text/html").vfuture
    } else {
      Results.NotFound("Not found !").vfuture
    }
  }
  def serverJs(api: Api, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    Results.Ok(
      s"""
         |(() => {
         |  const root = document.documentElement;
         |  const body = document.body;
         |  const prefix = body.dataset.portalPrefix || '';
         |  let lastMode = localStorage.getItem('portal-theme-mode');
         |
         |  const byId = (id) => document.getElementById(id);
         |  const prefixPath = (path) => prefix + path;
         |  const pretty = (value) => {
         |    try { return JSON.stringify(value, null, 2); } catch (e) { return String(value); }
         |  };
         |  const tryParseJSON = (str) => {
         |    if (!str || !str.trim()) return null;
         |    try { return JSON.parse(str); } catch (e) { throw new Error('Invalid JSON'); }
         |  };
         |  const copyToClipboard = async (text) => {
         |    try {
         |      if (navigator.clipboard && navigator.clipboard.writeText) {
         |        await navigator.clipboard.writeText(text);
         |        return true;
         |      }
         |      const area = document.createElement('textarea');
         |      area.value = text;
         |      document.body.appendChild(area);
         |      area.select();
         |      document.execCommand('copy');
         |      document.body.removeChild(area);
         |      return true;
         |    } catch (e) {
         |      return false;
         |    }
         |  };
         |
         |  const themeMedia = window.matchMedia('(prefers-color-scheme: dark)');
         |  const resolveTheme = (mode) => {
         |    if (mode === 'system') {
         |      return themeMedia.matches ? 'dark' : 'light';
         |    }
         |    return mode === 'dark' ? 'dark' : 'light';
         |  };
         |
         |  const applyThemeMode = (mode) => {
         |    const resolvedTheme = resolveTheme(mode);
         |    root.classList.toggle('dark', resolvedTheme === 'dark');
         |    const old = root.dataset.themeMode;
         |    root.dataset.themeMode = mode;
         |    localStorage.setItem('portal-theme-mode', mode);
         |    const select = byId('themeSelect');
         |    if (select) {
         |      select.value = mode;
         |    }
         |    if (lastMode !== mode) {
         |      lastMode = mode;
         |      window.location.reload();
         |    }
         |  };
         |
         |  const openModal = (id) => {
         |    const modal = byId(id);
         |    if (!modal) return;
         |    modal.classList.remove('hidden');
         |    modal.classList.add('flex');
         |    modal.setAttribute('aria-hidden', 'false');
         |    body.classList.add('overflow-hidden');
         |    const autofocus = modal.querySelector('[data-autofocus], input, textarea, select, button');
         |    if (autofocus) autofocus.focus();
         |  };
         |
         |  const closeModal = (id) => {
         |    const modal = byId(id);
         |    if (!modal) return;
         |    modal.classList.add('hidden');
         |    modal.classList.remove('flex');
         |    modal.setAttribute('aria-hidden', 'true');
         |    if (!document.querySelector('.portal-modal.flex')) {
         |      body.classList.remove('overflow-hidden');
         |    }
         |  };
         |
         |  window.portalOpenModal = openModal;
         |  window.portalCloseModal = closeModal;
         |
         |  document.addEventListener('click', async (event) => {
         |    const toggle = event.target.closest('[data-portal-toggle]');
         |    if (toggle) {
         |      const target = byId(toggle.getAttribute('data-portal-toggle'));
         |      if (target) {
         |        target.classList.toggle('hidden');
         |      }
         |      return;
         |    }
         |
         |    const openTrigger = event.target.closest('[data-modal-open]');
         |    if (openTrigger) {
         |      openModal(openTrigger.getAttribute('data-modal-open'));
         |      return;
         |    }
         |
         |    const closeTrigger = event.target.closest('[data-modal-close]');
         |    if (closeTrigger) {
         |      closeModal(closeTrigger.getAttribute('data-modal-close'));
         |      return;
         |    }
         |
         |    const backdrop = event.target.classList.contains('portal-modal') ? event.target : null;
         |    if (backdrop) {
         |      closeModal(backdrop.id);
         |      return;
         |    }
         |
         |    const copyBearer = event.target.closest('.apikey-bearer-copy');
         |    if (copyBearer) {
         |      await copyToClipboard(copyBearer.getAttribute('data-bearer') || '');
         |      return;
         |    }
         |
         |    const editApikey = event.target.closest('.apikey-edit');
         |    if (editApikey && window.openApiKeyEditor) {
         |      window.openApiKeyEditor({
         |        mode: 'update',
         |        apikey: {
         |          client_id: editApikey.getAttribute('data-client-id'),
         |          name: editApikey.getAttribute('data-name'),
         |          description: editApikey.getAttribute('data-description'),
         |          bearer: editApikey.getAttribute('data-bearer'),
         |          enabled: editApikey.getAttribute('data-enabled') === 'true',
         |          plan: editApikey.getAttribute('data-plan')
         |        }
         |      });
         |      return;
         |    }
         |
         |    const createApikey = event.target.closest('.apikey-create');
         |    if (createApikey && window.openApiKeyEditor) {
         |      window.openApiKeyEditor({ mode: 'create' });
         |      return;
         |    }
         |
         |    const deleteApikey = event.target.closest('.apikey-delete');
         |    if (deleteApikey) {
         |      const clientId = deleteApikey.getAttribute('data-client-id');
         |      const ok = window.confirm('Are you sure you want to delete this API key?');
         |      if (!ok) return;
         |      await fetch(prefixPath('/api/apikeys/' + encodeURIComponent(clientId)), {
         |        method: 'DELETE',
         |        credentials: 'include'
         |      });
         |      window.location.reload();
         |    }
         |  });
         |
         |  document.addEventListener('keydown', (event) => {
         |    if (event.key === 'Escape') {
         |      const opened = document.querySelector('.portal-modal.flex');
         |      if (opened) closeModal(opened.id);
         |    }
         |  });
         |
         |  const themeSelect = byId('themeSelect');
         |  if (themeSelect) {
         |    themeSelect.addEventListener('change', (event) => {
         |      applyThemeMode(event.target.value || 'system');
         |    });
         |  }
         |
         |  themeMedia.addEventListener('change', () => {
         |    if ((root.dataset.themeMode || 'system') === 'system') {
         |      applyThemeMode('system');
         |    }
         |  });
         |
         |  const setupApiKeyEditor = () => {
         |    const modalEl = byId('apiKeyEditorModal');
         |    if (!modalEl) return;
         |
         |    const titleEl = byId('apiKeyEditorTitle');
         |    const modeBadge = byId('apiKeyEditorModeBadge');
         |    const msgEl = byId('apiKeyEditorMsg');
         |    const nameEl = byId('apiKeyEditorName');
         |    const descEl = byId('apiKeyEditorDesc');
         |    const enabledEl = byId('apiKeyEditorEnabled');
         |    const planRow = byId('apiKeyEditorPlanRow');
         |    const planDisplay = byId('apiKeyEditorPlanDisplay');
         |    const planInput = byId('apiKeyEditorDisplay');
         |    const planSel = byId('apiKeyEditorPlan');
         |    const planHelp = byId('apiKeyEditorPlanHelp');
         |    const planHelp2 = byId('apiKeyEditorPlanHelp2');
         |    const cidRow = byId('apiKeyEditorClientIdRow');
         |    const cidEl = byId('apiKeyEditorClientId');
         |    const bearerRow = byId('apiKeyEditorBearerRow');
         |    const bearerEl = byId('apiKeyEditorBearer');
         |    const btnShow = byId('apiKeyEditorToggleBearer');
         |    const btnCopy = byId('apiKeyEditorCopyBearer');
         |    const btnSave = byId('apiKeyEditorSubmitBtn');
         |
         |    const setBusy = (busy) => {
         |      [nameEl, descEl, enabledEl, planSel, btnShow, btnCopy, btnSave].forEach((el) => {
         |        if (el) el.disabled = !!busy;
         |      });
         |    };
         |
         |    const showMsg = (text, kind) => {
         |      msgEl.textContent = text || '';
         |      msgEl.className = 'portal-feedback ' + (
         |        kind === 'success' ? 'portal-feedback-success' :
         |        kind === 'warning' ? 'portal-feedback-warning' :
         |        kind === 'danger' ? 'portal-feedback-danger' :
         |        'portal-feedback-muted'
         |      );
         |    };
         |
         |    const resetMsg = () => showMsg('', '');
         |
         |    const loadPlans = async () => {
         |      const mode = modeBadge.textContent || 'create';
         |      planSel.innerHTML = '<option value="">Choose a plan</option>';
         |      planHelp.textContent = '';
         |      try {
         |        const res = await fetch(prefixPath('/api/plans'), { method: 'GET', credentials: 'include' });
         |        if (!res.ok) throw new Error('Unable to load plans');
         |        const plans = await res.json();
         |        if (!Array.isArray(plans)) return;
         |        if (mode === 'create') {
         |          plans.forEach((plan) => {
         |            const opt = document.createElement('option');
         |            opt.value = plan.id;
         |            opt.textContent = plan.name || plan.id;
         |            opt.dataset.description = plan.description || '';
         |            planSel.appendChild(opt);
         |          });
         |          planSel.onchange = () => {
         |            const option = planSel.options[planSel.selectedIndex];
         |            planHelp.textContent = option && option.dataset ? option.dataset.description || '' : '';
         |          };
         |        } else {
         |          const plan = plans.find((item) => item.id === planInput.value);
         |          if (plan) {
         |            planInput.value = plan.name || plan.id;
         |            planHelp2.textContent = plan.description || '';
         |          }
         |        }
         |      } catch (e) {
         |        showMsg('Unable to load plans', 'warning');
         |      }
         |    };
         |
         |    btnShow.addEventListener('click', () => {
         |      bearerEl.type = bearerEl.type === 'password' ? 'text' : 'password';
         |    });
         |
         |    btnCopy.addEventListener('click', async () => {
         |      const ok = await copyToClipboard(bearerEl.value || '');
         |      showMsg(ok ? 'Bearer copied' : 'Copy failed', ok ? 'success' : 'danger');
         |      setTimeout(resetMsg, 1800);
         |    });
         |
         |    btnSave.addEventListener('click', async () => {
         |      resetMsg();
         |      const mode = modeBadge.textContent || 'create';
         |      setBusy(true);
         |      try {
         |        if (mode === 'create') {
         |          const payload = {
         |            name: nameEl.value.trim(),
         |            description: descEl.value.trim(),
         |            enabled: !!enabledEl.checked,
         |            plan: planSel.value
         |          };
         |          const res = await fetch(prefixPath('/api/apikeys'), {
         |            method: 'POST',
         |            headers: { 'content-type': 'application/json' },
         |            body: JSON.stringify(payload),
         |            credentials: 'include'
         |          });
         |          if (!res.ok) throw new Error('Create failed');
         |          showMsg('API key created', 'success');
         |          setTimeout(() => {
         |            closeModal('apiKeyEditorModal');
         |            window.location.reload();
         |          }, 600);
         |        } else {
         |          const clientId = cidEl.value;
         |          const payload = {
         |            name: nameEl.value.trim(),
         |            description: descEl.value.trim(),
         |            enabled: !!enabledEl.checked
         |          };
         |          const res = await fetch(prefixPath('/api/apikeys/' + encodeURIComponent(clientId)), {
         |            method: 'PUT',
         |            headers: { 'content-type': 'application/json' },
         |            body: JSON.stringify(payload),
         |            credentials: 'include'
         |          });
         |          if (!res.ok) throw new Error('Update failed');
         |          showMsg('API key updated', 'success');
         |          setTimeout(() => {
         |            closeModal('apiKeyEditorModal');
         |            window.location.reload();
         |          }, 600);
         |        }
         |      } catch (e) {
         |        showMsg(e.message || 'Unexpected error', 'danger');
         |      } finally {
         |        setBusy(false);
         |      }
         |    });
         |
         |    window.openApiKeyEditor = async ({ mode = 'create', apikey = null } = {}) => {
         |      resetMsg();
         |      nameEl.value = '';
         |      descEl.value = '';
         |      enabledEl.checked = true;
         |      planSel.value = '';
         |      planInput.value = '';
         |      planHelp.textContent = '';
         |      planHelp2.textContent = '';
         |
         |      if (mode === 'update') {
         |        titleEl.textContent = 'Update API key';
         |        modeBadge.textContent = 'update';
         |        modeBadge.hidden = false;
         |        cidRow.hidden = false;
         |        bearerRow.hidden = false;
         |        planRow.hidden = true;
         |        planDisplay.hidden = false;
         |        const key = apikey || {};
         |        cidEl.value = key.client_id || '';
         |        nameEl.value = key.name || '';
         |        descEl.value = key.description || '';
         |        enabledEl.checked = !!key.enabled;
         |        planInput.value = key.plan || '';
         |        bearerEl.value = key.bearer || '';
         |        bearerEl.type = 'password';
         |      } else {
         |        titleEl.textContent = 'Create API key';
         |        modeBadge.textContent = 'create';
         |        modeBadge.hidden = true;
         |        cidRow.hidden = true;
         |        bearerRow.hidden = true;
         |        planRow.hidden = false;
         |        planDisplay.hidden = true;
         |      }
         |
         |      await loadPlans();
         |      openModal('apiKeyEditorModal');
         |    };
         |  };
         |
         |  const setupApiTester = () => {
         |    const modalEl = byId('apiTesterModal');
         |    if (!modalEl) return;
         |
         |    const methodBadge = byId('apiTesterMethod');
         |    const urlInput = byId('apiTesterUrl');
         |    const secretWarn = byId('apiTesterSecretWarn');
         |    const apiKeySelect = byId('apiTesterApiKeySelect');
         |    const headersArea = byId('apiTesterHeaders');
         |    const bodyArea = byId('apiTesterBody');
         |    const statusEl = byId('apiTesterStatus');
         |    const durationEl = byId('apiTesterDuration');
         |    const responseHeadersEl = byId('apiTesterRespHeaders');
         |    const responseBodyEl = byId('apiTesterRespBody');
         |    const errorEl = byId('apiTesterError');
         |    const btnSend = byId('apiTesterSendBtn');
         |    const btnResetHeaders = byId('apiTesterResetHeaders');
         |    const btnClearResponse = byId('apiTesterClearResponse');
         |    const defaultHeaders = { accept: 'application/json' };
         |
         |    const updateSecretBanner = () => {
         |      const content = (headersArea.value || '') + ' ' + (bodyArea.value || '');
         |      const danger = /authorization|secret|token|apikey|api[-_ ]key/i.test(content);
         |      secretWarn.classList.toggle('hidden', !danger);
         |    };
         |
         |    const loadApiKeys = async () => {
         |      apiKeySelect.innerHTML = '<option value="">No API key</option>';
         |      try {
         |        const res = await fetch(prefixPath('/api/apikeys'), { method: 'GET', credentials: 'include' });
         |        if (!res.ok) return;
         |        const keys = await res.json();
         |        if (!Array.isArray(keys)) return;
         |        keys.forEach((key, index) => {
         |          const option = document.createElement('option');
         |          option.value = String(index);
         |          option.textContent = (key.name || 'API key') + ' (' + (key.client_id || '-') + ')';
         |          option.dataset.bearer = key.bearer || '';
         |          apiKeySelect.appendChild(option);
         |        });
         |      } catch (e) {
         |        console.warn('Unable to load API keys', e);
         |      }
         |    };
         |
         |    apiKeySelect.addEventListener('change', () => {
         |      const option = apiKeySelect.options[apiKeySelect.selectedIndex];
         |      const bearer = option && option.dataset ? option.dataset.bearer || '' : '';
         |      let headers = defaultHeaders;
         |      try {
         |        headers = tryParseJSON(headersArea.value) || { ...defaultHeaders };
         |      } catch (e) {
         |        headers = { ...defaultHeaders };
         |      }
         |      if (bearer) headers.authorization = 'Bearer ' + bearer;
         |      headersArea.value = pretty(headers);
         |      updateSecretBanner();
         |    });
         |
         |    headersArea.addEventListener('input', updateSecretBanner);
         |    bodyArea.addEventListener('input', updateSecretBanner);
         |
         |    btnResetHeaders.addEventListener('click', () => {
         |      headersArea.value = pretty(defaultHeaders);
         |      updateSecretBanner();
         |    });
         |
         |    btnClearResponse.addEventListener('click', () => {
         |      statusEl.textContent = '-';
         |      durationEl.textContent = '-';
         |      responseHeadersEl.textContent = '';
         |      responseBodyEl.textContent = '';
         |      errorEl.textContent = '';
         |    });
         |
         |    btnSend.addEventListener('click', async () => {
         |      errorEl.textContent = '';
         |      statusEl.textContent = '...';
         |      durationEl.textContent = '...';
         |      responseHeadersEl.textContent = '';
         |      responseBodyEl.textContent = '';
         |
         |      let headers;
         |      let requestBody;
         |      try {
         |        headers = tryParseJSON(headersArea.value) || {};
         |      } catch (e) {
         |        errorEl.textContent = 'Invalid headers JSON';
         |        return;
         |      }
         |      try {
         |        requestBody = tryParseJSON(bodyArea.value);
         |      } catch (e) {
         |        errorEl.textContent = 'Invalid body JSON';
         |        return;
         |      }
         |
         |      const started = performance.now();
         |      try {
         |        const res = await fetch(prefixPath('/api/_test'), {
         |          method: 'POST',
         |          credentials: 'include',
         |          headers: { 'content-type': 'application/json' },
         |          body: JSON.stringify({
         |            method: methodBadge.textContent,
         |            url: urlInput.value,
         |            headers: headers,
         |            body_json: requestBody
         |          })
         |        });
         |        const payload = await res.json();
         |        durationEl.textContent = Math.round(performance.now() - started) + ' ms';
         |        statusEl.textContent = String(payload.status || '-') + ' ' + String(payload.statusText || '');
         |        responseHeadersEl.textContent = pretty(payload.headers || {});
         |        const responseHeaders = payload.headers || {};
         |        const contentType = responseHeaders['content-type'] || responseHeaders['Content-Type'] || '';
         |        if (contentType.indexOf('json') > -1) {
         |          try {
         |            responseBodyEl.textContent = pretty(JSON.parse(payload.body_str || '{}'));
         |          } catch (e) {
         |            responseBodyEl.textContent = payload.body_str || '';
         |          }
         |        } else {
         |          responseBodyEl.textContent = payload.body_str || '';
         |        }
         |      } catch (e) {
         |        durationEl.textContent = Math.round(performance.now() - started) + ' ms';
         |        statusEl.textContent = '-';
         |        errorEl.textContent = e.message || 'Request failed';
         |      }
         |    });
         |
         |    window.openApiTester = async ({ url, method = 'GET', presetHeaders = null, presetBody = null } = {}) => {
         |      methodBadge.textContent = String(method || 'GET').toUpperCase();
         |      urlInput.value = url || '';
         |      headersArea.value = pretty(presetHeaders && typeof presetHeaders === 'object' ? presetHeaders : defaultHeaders);
         |      if (typeof presetBody === 'string') {
         |        bodyArea.value = presetBody;
         |      } else if (presetBody) {
         |        bodyArea.value = pretty(presetBody);
         |      } else {
         |        bodyArea.value = '';
         |      }
         |      statusEl.textContent = '-';
         |      durationEl.textContent = '-';
         |      responseHeadersEl.textContent = '';
         |      responseBodyEl.textContent = '';
         |      errorEl.textContent = '';
         |      await loadApiKeys();
         |      updateSecretBanner();
         |      openModal('apiTesterModal');
         |    };
         |  };
         |
         |  const preferredThemeMode = localStorage.getItem('portal-theme-mode') || 'system';
         |  applyThemeMode(preferredThemeMode);
         |  setupApiKeyEditor();
         |  setupApiTester();
         |})();
         |""".stripMargin
    ).as("text/javascript").vfuture
  }
  def servePlansJson(api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    Results.Ok(JsArray(api.plans.map(_.raw))).vfuture
  }
  def serveDocumentationJson(api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    Results.Ok(doc.json.asObject - "source" ++ Json.obj(
      "name" -> api.name,
      "description" -> api.description,
      "api_tags" -> api.tags,
      "api_metadata" -> api.metadata,
      "doc_tags" -> doc.tags,
      "doc_metadata" -> doc.metadata,
    )).vfuture
  }
  def serveApikeysPage(api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    val publishedPlans = api.plans.filter(c => c.status == ApiPlanStatus.Published)
    Source(publishedPlans.toList).mapAsync(1) { plan =>
      apikeysFromApiForUser(plan, ctx).map(_.map(t => (t._1, t._2, plan)))
    }
    .flatMapConcat(seq => Source(seq.toList))
    .runWith(Sink.seq)
    .flatMap { apikeys =>
      val waitingCount = apikeys.count(_._2.metadata.get("waiting-approval").contains("true"))
      val activeCount = apikeys.count(_._2.enabled)
      val sidebar = ApiDocumentationSidebar(Json.obj(
        "items" -> Json.arr(Json.obj(
          "label" -> "My apikeys",
          "link" -> s"${config.prefix.getOrElse("")}/subscriptions/apikeys",
          "icon" -> Json.obj("css_icon_class" -> "bi bi-key")
        ))
      ))
      Results.Ok(documentationPageTemplate(s"${api.name} - Subscriptions", config.prefix.getOrElse(""), api, doc, sidebar, ctx)(
        s"""<div class="space-y-6">
           |  <section class="flex flex-col gap-5 rounded-[28px] border border-slate-200/80 bg-gradient-to-br from-white to-stone-100 p-7 shadow-sm lg:flex-row lg:items-end lg:justify-between dark:border-white/10 dark:from-slate-900 dark:to-slate-900/70">
           |    <div>
           |      <span class="portal-eyebrow">Access</span>
           |      <h1 class="portal-page-title">Subscriptions</h1>
           |      <p class="portal-page-subtitle">Manage credentials, approval status and quick access tokens for this API.</p>
           |    </div>
           |    <div>
           |      <button type="button" class="apikey-create inline-flex items-center gap-2 rounded-full bg-slate-950 px-4 py-2.5 text-sm font-semibold text-white shadow-sm transition hover:-translate-y-0.5 dark:bg-white dark:text-slate-950">
           |        <i class="bi bi-plus-circle"></i>
           |        <span>Create API key</span>
           |      </button>
           |    </div>
           |  </section>
           |
           |  <section class="grid gap-4 sm:grid-cols-2 xl:grid-cols-4">
           |    <article class="portal-shell-card p-5">
           |      <span class="text-sm text-slate-500 dark:text-slate-400">Published plans</span>
           |      <strong class="mt-3 block text-3xl font-semibold tracking-tight text-slate-950 dark:text-white">${publishedPlans.size}</strong>
           |    </article>
           |    <article class="portal-shell-card p-5">
           |      <span class="text-sm text-slate-500 dark:text-slate-400">API keys</span>
           |      <strong class="mt-3 block text-3xl font-semibold tracking-tight text-slate-950 dark:text-white">${apikeys.size}</strong>
           |    </article>
           |    <article class="portal-shell-card p-5">
           |      <span class="text-sm text-slate-500 dark:text-slate-400">Active</span>
           |      <strong class="mt-3 block text-3xl font-semibold tracking-tight text-slate-950 dark:text-white">${activeCount}</strong>
           |    </article>
           |    <article class="portal-shell-card p-5">
           |      <span class="text-sm text-slate-500 dark:text-slate-400">Waiting approval</span>
           |      <strong class="mt-3 block text-3xl font-semibold tracking-tight text-slate-950 dark:text-white">${waitingCount}</strong>
           |    </article>
           |  </section>
           |
           |  <section class="portal-surface">
           |    <div class="mb-6">
           |      <div>
           |        <h2 class="text-xl font-semibold tracking-tight text-slate-950 dark:text-white">Your credentials</h2>
           |        <p class="mt-2 text-sm leading-6 text-slate-600 dark:text-slate-300">Use these keys in the API tester or in your own applications.</p>
           |      </div>
           |    </div>
           |    <div class="overflow-auto">
           |      <table class="min-w-full border-collapse text-left">
           |        <thead>
           |          <tr>
           |            <th class="border-b border-slate-200 px-4 py-3 text-[11px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:border-white/10 dark:text-slate-400">#</th>
           |            <th class="border-b border-slate-200 px-4 py-3 text-[11px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:border-white/10 dark:text-slate-400">Name</th>
           |            <th class="border-b border-slate-200 px-4 py-3 text-[11px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:border-white/10 dark:text-slate-400">Plan</th>
           |            <th class="border-b border-slate-200 px-4 py-3 text-[11px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:border-white/10 dark:text-slate-400">Enabled</th>
           |            <th class="border-b border-slate-200 px-4 py-3 text-[11px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:border-white/10 dark:text-slate-400">Status</th>
           |            <th class="border-b border-slate-200 px-4 py-3 text-[11px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:border-white/10 dark:text-slate-400">Actions</th>
           |          </tr>
           |        </thead>
           |        <tbody>
           |          ${apikeys.zipWithIndex.map { tuple =>
                        val waiting = tuple._1._2.metadata.get("waiting-approval").contains("true")
                        val statusPill =
                          if (waiting) """<span class="portal-pill portal-pill-warning">waiting approval</span>"""
                          else """<span class="portal-pill portal-pill-success">approved</span>"""
                        val enabledPill =
                          if (tuple._1._2.enabled) """<span class="portal-pill portal-pill-success">enabled</span>"""
                          else """<span class="portal-pill portal-pill-muted">disabled</span>"""
                        val copyAction =
                          if (waiting) ""
                          else s"""<button class="apikey-bearer-copy inline-flex items-center gap-2 rounded-full border border-slate-200 bg-white px-3 py-2 text-xs font-semibold text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" type="button" title="Copy bearer" data-bearer="${tuple._1._2.toBearer()}"><i class="bi bi-copy"></i><span>Copy</span></button>"""
                        s"""<tr>
                           |  <td class="border-b border-slate-200 px-4 py-4 text-sm text-slate-500 dark:border-white/10 dark:text-slate-400">${tuple._2 + 1}</td>
                           |  <td class="border-b border-slate-200 px-4 py-4 dark:border-white/10">
                           |    <div class="font-semibold text-slate-950 dark:text-white">${tuple._1._2.clientName}</div>
                           |    <div class="mt-1 font-mono text-xs text-slate-500 dark:text-slate-400">${tuple._1._2.clientId}</div>
                           |  </td>
                           |  <td class="border-b border-slate-200 px-4 py-4 text-sm text-slate-700 dark:border-white/10 dark:text-slate-200">${tuple._1._3.name}</td>
                           |  <td class="border-b border-slate-200 px-4 py-4 dark:border-white/10">${enabledPill}</td>
                           |  <td class="border-b border-slate-200 px-4 py-4 dark:border-white/10">${statusPill}</td>
                           |  <td class="border-b border-slate-200 px-4 py-4 dark:border-white/10">
                           |    <div class="flex flex-wrap gap-2">
                           |      <button class="apikey-edit inline-flex items-center gap-2 rounded-full border border-slate-200 bg-white px-3 py-2 text-xs font-semibold text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" type="button" title="Edit API key"
                           |        data-client-id="${tuple._1._2.clientId}"
                           |        data-name="${tuple._1._2.clientName}"
                           |        data-description="${tuple._1._2.description}"
                           |        data-bearer="${if (waiting) "--" else tuple._1._2.toBearer()}"
                           |        data-enabled="${tuple._1._2.enabled}"
                           |        data-plan="${tuple._1._3.id}"
                           |        data-sub="${tuple._1._1.id}">
                           |        <i class="bi bi-pencil-square"></i>
                           |        <span>Edit</span>
                           |      </button>
                           |      ${copyAction}
                           |      <button class="apikey-delete inline-flex items-center gap-2 rounded-full border border-rose-200 bg-rose-50 px-3 py-2 text-xs font-semibold text-rose-700 shadow-sm transition hover:-translate-y-0.5 dark:border-rose-500/20 dark:bg-rose-500/10 dark:text-rose-300" type="button" title="Delete API key"
                           |        data-client-id="${tuple._1._2.clientId}"
                           |        data-sub="${tuple._1._1.id}"
                           |        data-plan="${tuple._1._3.id}">
                           |        <i class="bi bi-trash"></i>
                           |        <span>Delete</span>
                           |      </button>
                           |    </div>
                           |  </td>
                           |</tr>""".stripMargin
                      }.mkString("\n")}
           |        </tbody>
           |      </table>
           |    </div>
           |  </section>
           |</div>
           |""".stripMargin)).as("text/html").vfuture
    }
  }

  def serveAllApikeys(api: Api, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    Source(api.plans.filter(c => c.status == ApiPlanStatus.Published).toList).mapAsync(1) { plan =>
      apikeysFromApiForUser(plan, ctx).map(_.map(t => (t._1, t._2, plan)))
    }
    .flatMapConcat(s => Source(s.toList))
    .runWith(Sink.seq)
    .flatMap { apikeys =>
      Results.Ok(JsArray(apikeys.map {
        case (sub, key, plan) =>
          key.metadata.get("waiting-approval") match {
            case Some("true") => Json.obj(
              "client_id" -> key.clientId,
              "name" -> key.clientName,
              "description" -> key.description,
              "bearer" -> "--",
              "enabled" -> false,
              "sub" -> sub.id,
              "plan" -> plan.id,
              "status" -> "waiting-approval",
            )
            case _ => Json.obj(
              "client_id" -> key.clientId,
              "name" -> key.clientName,
              "description" -> key.description,
              "bearer" -> key.toBearer(),
              "enabled" -> key.enabled,
              "sub" -> sub.id,
              "plan" -> plan.id,
              "status" -> "validated",
            )
          }
      })).as("application/json").vfuture
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
          "statusText" -> resp.statusText,
          "headers" -> resp.headers.mapValues(_.last),
          "body_str" -> resp.body[String]
        ))
      }
    }
  }
  def serveCreateApikey(api: Api, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    ctx.request.body.runFold(ByteString.empty)(_ ++ _).flatMap { body =>
      val bodyJson = Json.parse(body.utf8String)
      val nameOpt = bodyJson.select("name").asOpt[String]
      val descriptionOpt = bodyJson.select("description").asOpt[String]
      val enabledOpt = bodyJson.select("enabled").asOpt[Boolean]
      val planOpt = bodyJson.select("plan").asOpt[String]
      val clientName = nameOpt.getOrElse("New apikey")
      (for {
        user <- ctx.user
        doc <- api.documentation
        plan <- api.plans.find(p => planOpt.contains(p.id)).orElse(api.plans.headOption)
      } yield {
        val sub_id = s"api-plan-subscription_${IdGenerator.uuid}"
        val clientId = IdGenerator.lowerCaseToken(16)
        val sub = ApiSubscription(
          location = api.location,
          id = sub_id,
          name = s"subscription - ${clientName}",
          description = descriptionOpt.getOrElse(""),
          status = ApiSubscriptionEnabled,
          dates = ApiSubscriptionDates(
            created_at = DateTime.now(),
            processed_at = DateTime.now(),
            started_at = DateTime.now(),
            paused_at = DateTime.now(),
            ending_at = DateTime.now(),
            closed_at = DateTime.now(),
          ),
          ownerRef = user.email,
          apiRef = api.id,
          subscriptionKind = plan.accessModeConfiguration.map(_.apiKind).getOrElse(ApiKind.Keyless),
          tokenRefs = if (plan.validation.isAuto) Seq(Json.obj("apikey" -> clientId)) else Seq.empty,
          planRef = plan.id,
          tags = Seq.empty,
          metadata = Map(
            "created_by" -> "otoroshi-api-portal-plugin",
            "otoroshi-api-plan-ref" -> plan.id,
            "otoroshi-api-plan-name" -> plan.name,
            "otoroshi-api-client-name" -> clientName,
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
          throttlingQuota = plan.rateLimiting.map(_.quota.window).getOrElse(RemainingQuotas.MaxValue),
          dailyQuota = plan.rateLimiting.map(_.quota.daily).getOrElse(RemainingQuotas.MaxValue),
          monthlyQuota = plan.rateLimiting.map(_.quota.monthly).getOrElse(RemainingQuotas.MaxValue),
          tags = plan.tags,
          metadata = plan.metadata ++ Map(
            "created_by" -> "otoroshi-api-portal-plugin",
            "otoroshi-api-ref" -> api.id,
            "otoroshi-api-sub-ref" -> sub.id,
            "otoroshi-api-plan-ref" -> plan.id,
            "otoroshi-api-plan-name" -> plan.name,
            "created_at" -> DateTime.now().toString(),
            "updated_at" -> DateTime.now().toString(),
          ),
          location = api.location,
        )
        if (env.clusterConfig.mode.isWorker) {
          if (plan.validation.isAuto) {
            ClusterAgent.clusterSaveApikey(env, newApikey)(ec, env.otoroshiMaterializer)
          }
          // TODO: implement it
          // ClusterAgent.clusterSaveSub(env, sub)(ec, env.otoroshiMaterializer)
          // ClusterAgent.clusterSaveApi(env, sub)(ec, env.otoroshiMaterializer)
        }
        // TODO: right now api plan does not store subscription refs but maybe we will need it
        // env.datastores.apiDataStore.set(api.copy(consumers = api.consumers.map { c =>
        //   if (c.id == consumer.id) {
        //     c.copy(
        //       subscriptions = c.subscriptions :+ ApiConsumerSubscriptionRef(sub_id),
        //     )
        //   } else {
        //     c
        //   }
        // })).flatMap { _ =>
          env.datastores.apiSubscriptionDataStore.set(sub).flatMap { _ =>
            if (plan.validation.isAuto) {
              env.datastores.apiKeyDataStore.set(newApikey).map { _ =>
                Results.Ok(Json.obj(
                  "client_id" -> newApikey.clientId,
                  "name" -> newApikey.clientName,
                  "description" -> newApikey.description,
                  "bearer" -> newApikey.toBearer(),
                  "enabled" -> newApikey.enabled,
                  "sub" -> sub.id,
                  "plan" -> plan.id,
                  "status" -> "validated",
                )).as("application/json")
              }
            } else {
              Results.Ok(Json.obj(
                "client_id" -> newApikey.clientId,
                "name" -> newApikey.clientName,
                "description" -> newApikey.description,
                "bearer" -> "--",
                "enabled" -> false,
                "sub" -> sub.id,
                "plan" -> plan.id,
                "status" -> "waiting-approval",
              )).as("application/json").vfuture
            }
          }
        //}
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
          apikey.metadata.get("otoroshi-api-sub-ref") match {
            case None => Results.Unauthorized(Json.obj("error" -> "sref not found")).vfuture
            case Some(sub_id) =>
              env.datastores.apiSubscriptionDataStore.findById(sub_id).flatMap {
                case None => Results.Unauthorized(Json.obj("error" -> "sub not found")).vfuture
                case Some(sub) if sub.apiRef != api.id => Results.Unauthorized(Json.obj("error" -> "bad sub")).vfuture
                case Some(sub) => {
                  if (env.clusterConfig.mode.isWorker) {
                    ClusterAgent.clusterDeleteApikey(env, apikey.clientId)(ec, env.otoroshiMaterializer)
                    // TODO: implement it
                    // ClusterAgent.clusterDeleteSub(env, sub)(ec, env.otoroshiMaterializer)
                    // ClusterAgent.clusterDeleteApi(env, sub)(ec, env.otoroshiMaterializer)
                  }
                  // TODO: here plans don't store sub refs
                  //env.datastores.apiDataStore.set(api.copy(consumers = api.consumers.map { c =>
                  //  if (c.id == consumer.id) {
                  //    c.copy(
                  //      subscriptions = c.subscriptions.filterNot(_.ref == sub_id),
                  //    )
                  //  } else {
                  //    c
                  //  }
                  //})).flatMap { _ =>
                  env.datastores.apiSubscriptionDataStore.delete(sub.id).flatMap { _ =>
                    env.datastores.apiKeyDataStore.delete(apikey.clientId).map { _ =>
                      Results.NoContent
                    }
                  }
                  //}
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
      val nameOpt = bodyJson.select("name").asOpt[String]
      val descriptionOpt = bodyJson.select("description").asOpt[String]
      val enabledOpt = bodyJson.select("enabled").asOpt[Boolean]
      ctx.user match {
        case None => Results.Unauthorized(Json.obj("error" -> "user not found")).vfuture
        case Some(user) => env.datastores.apiKeyDataStore.findById(client_id).flatMap {
          case None => Results.Unauthorized(Json.obj("error" -> "apikey not found")).vfuture
          case Some(apikey) => {
            apikey.metadata.get("otoroshi-api-plan-ref") match {
              case None => Results.Unauthorized(Json.obj("error" -> "cref not found")).vfuture
              case Some(plan_id) => apikey.metadata.get("otoroshi-api-sub-ref") match {
                case None => Results.Unauthorized(Json.obj("error" -> "sref not found")).vfuture
                case Some(sub_id) => api.plans.filter(c => c.status == ApiPlanStatus.Published).find(_.id == plan_id) match {
                  case None => Results.Unauthorized(Json.obj("error" -> "plan not found")).vfuture
                  case Some(plan) => Option(sub_id) match { // TODO: need to store sub refs on plan ?
                    case None => Results.Unauthorized(Json.obj("error" -> "subref not found")).vfuture
                    case Some(subRef) => env.datastores.apiSubscriptionDataStore.findById(subRef).flatMap {
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
                        env.datastores.apiSubscriptionDataStore.set(sub.copy(name = s"subscription - ${newApikey.clientName}", description = newApikey.description)).flatMap { _ =>
                          if (plan.validation.isAuto && sub.status == ApiSubscriptionEnabled) {
                            env.datastores.apiKeyDataStore.set(newApikey).map { _ =>
                              Results.Ok(Json.obj(
                                "client_id" -> newApikey.clientId,
                                "name" -> newApikey.clientName,
                                "description" -> newApikey.description,
                                "bearer" -> newApikey.toBearer(),
                                "enabled" -> newApikey.enabled,
                                "sub" -> sub.id,
                                "plan" -> plan.id,
                                "status" -> "validated"
                              )).as("application/json")
                            }
                          } else {
                            Results.Ok(Json.obj(
                              "client_id" -> newApikey.clientId,
                              "name" -> newApikey.clientName,
                              "description" -> newApikey.description,
                              "bearer" -> "--",
                              "enabled" -> false,
                              "sub" -> sub.id,
                              "plan" -> plan.id,
                              "status" -> "waiting-approval",
                            )).as("application/json").vfuture
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

  def renderResourceAsIcon(resource: Option[ApiDocumentationResource], doc: ApiDocumentation, style: Option[String] = None): String = {
    resource match {
      case Some(resource) if resource.css_icon_class.isDefined => s"""<i class="${resource.css_icon_class.get}"></i> """
      case Some(resource) if resource.text_content.exists(_.startsWith("bi ")) => s"""<i class="${resource.text_content.get}"></i> """
      case Some(resource) if resource.text_content.isDefined => s"""<span class="portal-icon-text">${resource.text_content.get}</span> """
      case Some(resource) if resource.resolveUrl(doc).isDefined => s"""<img src="${resource.resolveUrl(doc).get}" style="${resource.raw.select("style").asOptString.orElse(style).getOrElse("")}" /> """
      case Some(resource) if resource.base64_content.isDefined => s"""<img src="data:${resource.contentType};base64,${resource.base64_content.get}" style="${resource.raw.select("style").asOptString.orElse(style).getOrElse("")}" /> """
      case _ => ""
    }
  }

  def renderResource(resource: ApiDocumentationResource, doc: ApiDocumentation)(implicit  env: Env, ec: ExecutionContext, mat: Materializer): Future[(ByteString, String)] = {
    def handleTransform(byteString: ByteString): ByteString = {
      // TODO: handle EL
      if (resource.transform.contains("markdown")) {
        val content =
          s"""<zero-md class="portal-markdown">
             |  <template>
             |    <style>
             |      :host {
             |        display: block;
             |        color: #0f172a;
             |      }
             |
             |      .markdown-body {
             |        background: transparent !important;
             |        color: #334155 !important;
             |        font-family: "Manrope", ui-sans-serif, system-ui, sans-serif !important;
             |        line-height: 1.75 !important;
             |      }
             |
             |      .markdown-body h1,
             |      .markdown-body h2,
             |      .markdown-body h3,
             |      .markdown-body h4,
             |      .markdown-body h5,
             |      .markdown-body h6 {
             |        color: #020617 !important;
             |        letter-spacing: -0.03em;
             |        border-bottom-color: rgba(148, 163, 184, 0.22) !important;
             |      }
             |
             |      .markdown-body a {
             |        color: #0284c7 !important;
             |      }
             |
             |      .markdown-body p,
             |      .markdown-body li,
             |      .markdown-body blockquote {
             |        color: #475569 !important;
             |      }
             |
             |      .markdown-body pre {
             |        background: #020617 !important;
             |        color: #e2e8f0 !important;
             |        border-radius: 1rem !important;
             |        padding: 1rem !important;
             |      }
             |
             |      .markdown-body code {
             |        font-family: "IBM Plex Mono", ui-monospace, SFMono-Regular, monospace !important;
             |      }
             |
             |      .markdown-body :not(pre) > code {
             |        background: rgba(148, 163, 184, 0.14) !important;
             |        color: #0f172a !important;
             |        border-radius: 0.5rem !important;
             |        padding: 0.15rem 0.4rem !important;
             |      }
             |
             |      .markdown-body table th,
             |      .markdown-body table td {
             |        border-color: rgba(148, 163, 184, 0.22) !important;
             |      }
             |
             |      .markdown-body blockquote {
             |        border-left-color: rgba(14, 165, 233, 0.35) !important;
             |      }
             |
             |      :host-context(.dark) {
             |        color: #f8fafc;
             |      }
             |
             |      :host-context(.dark) .markdown-body {
             |        background: transparent !important;
             |        color: #cbd5e1 !important;
             |      }
             |
             |      :host-context(.dark) .markdown-body h1,
             |      :host-context(.dark) .markdown-body h2,
             |      :host-context(.dark) .markdown-body h3,
             |      :host-context(.dark) .markdown-body h4,
             |      :host-context(.dark) .markdown-body h5,
             |      :host-context(.dark) .markdown-body h6 {
             |        color: #f8fafc !important;
             |        border-bottom-color: rgba(255, 255, 255, 0.08) !important;
             |      }
             |
             |      :host-context(.dark) .markdown-body p,
             |      :host-context(.dark) .markdown-body li,
             |      :host-context(.dark) .markdown-body blockquote {
             |        color: #cbd5e1 !important;
             |      }
             |
             |      :host-context(.dark) .markdown-body a {
             |        color: #38bdf8 !important;
             |      }
             |
             |      :host-context(.dark) .markdown-body pre {
             |        background: #020617 !important;
             |        color: #e2e8f0 !important;
             |      }
             |
             |      :host-context(.dark) .markdown-body :not(pre) > code {
             |        background: rgba(255, 255, 255, 0.08) !important;
             |        color: #f8fafc !important;
             |      }
             |
             |      :host-context(.dark) .markdown-body table th,
             |      :host-context(.dark) .markdown-body table td {
             |        border-color: rgba(255, 255, 255, 0.08) !important;
             |      }
             |
             |      :host-context(.dark) .markdown-body blockquote {
             |        border-left-color: rgba(56, 189, 248, 0.35) !important;
             |      }
             |    </style>
             |  </template>
             |  <script type="text/markdown">${byteString.utf8String}</script>
             |</zero-md>""".stripMargin
        resource.transform_wrapper match {
          case None => ByteString(content)
          case Some(wrapper) => ByteString(wrapper.replace("{content}", content))
        }
      } else if (resource.transform.contains("redoc")) {
        val content = s"""<redoc
                      |   spec-url="${resource.path.headOption.getOrElse("#")}"
                      |   hideHostname="false"
                      |   showObjectSchemaExamples="true"
                      |></redoc>""".stripMargin
        resource.transform_wrapper match {
          case None => ByteString(content)
          case Some(wrapper) => ByteString(wrapper.replace("{content}", content))
        }
      } else {
        byteString
      }
    }
    resource.resolveUrl(doc) match {
      case Some(url) => env.Ws.url(url)
        .withFollowRedirects(resource.httpFollowRedirects)
        .withHttpHeaders(resource.httpHeaders.toSeq: _*)
        .withRequestTimeout(resource.httpTimeout)
        .get() map { resp =>
          (handleTransform(resp.bodyAsBytes), resource.contentType)
        }
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
    val sidebarId = "portalDocSidebar"
    val sidebarHtml =
      s"""<aside class="xl:sticky xl:top-28 xl:self-start">
         |  <div class="rounded-3xl border border-slate-200/80 bg-white/80 p-5 shadow-sm backdrop-blur dark:border-white/10 dark:bg-slate-900/70">
         |    <div class="mb-4 flex items-center justify-between gap-3">
         |      <div>
         |        <p class="text-[11px] font-bold uppercase tracking-[0.18em] text-sky-600 dark:text-sky-400">Browse</p>
         |        <h2 class="mt-2 text-base font-semibold tracking-tight text-slate-950 dark:text-white">${sidebar.label}</h2>
         |      </div>
         |      <button class="inline-flex items-center gap-2 rounded-full border border-slate-200 bg-white px-3 py-2 text-sm font-medium text-slate-700 shadow-sm xl:hidden dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" type="button" data-portal-toggle="${sidebarId}">
         |        <i class="bi bi-list"></i>
         |        <span>Sections</span>
         |      </button>
         |    </div>
         |    <nav id="${sidebarId}" class="hidden space-y-3 xl:block">
         |      ${sidebarTemplate(sidebar, prefix, doc, ctx)}
         |    </nav>
         |  </div>
         |</aside>""".stripMargin
    val railHtml =
      s"""<aside class="xl:sticky xl:top-28 xl:self-start">
         |  <div class="rounded-3xl border border-slate-200/80 bg-white/80 p-5 shadow-sm backdrop-blur dark:border-white/10 dark:bg-slate-900/70">
         |    <p class="text-[11px] font-bold uppercase tracking-[0.18em] text-sky-600 dark:text-sky-400">Quick links</p>
         |    <h2 class="mt-2 text-base font-semibold tracking-tight text-slate-950 dark:text-white">Keep moving</h2>
         |    <p class="mt-2 text-sm leading-6 text-slate-600 dark:text-slate-400">Jump between guides, reference material and self-service access without losing context.</p>
         |    <div class="mt-5 space-y-3">
         |      <a class="inline-flex w-full items-center justify-center gap-2 rounded-full bg-slate-950 px-4 py-2.5 text-sm font-semibold text-white shadow-sm transition hover:-translate-y-0.5 dark:bg-white dark:text-slate-950" href="${prefix}/api-references">API reference</a>
         |      ${if (ctx.user.isDefined && api.plans.exists(_.accessModeConfiguration.map(_.apiKind).getOrElse(ApiKind.Keyless) != ApiKind.Keyless)) {
              s"""<a class="inline-flex w-full items-center justify-center gap-2 rounded-full border border-slate-200 bg-white px-4 py-2.5 text-sm font-semibold text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" href="${prefix}/subscriptions">Subscriptions</a>"""
            } else ""}
         |    </div>
         |    <dl class="mt-6 grid grid-cols-3 gap-3">
         |      <div class="rounded-2xl bg-stone-100/80 px-3 py-3 dark:bg-white/5">
         |        <dt class="text-[10px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:text-slate-400">Specs</dt>
         |        <dd class="mt-2 text-lg font-semibold text-slate-950 dark:text-white">${doc.references.size}</dd>
         |      </div>
         |      <div class="rounded-2xl bg-stone-100/80 px-3 py-3 dark:bg-white/5">
         |        <dt class="text-[10px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:text-slate-400">Plans</dt>
         |        <dd class="mt-2 text-lg font-semibold text-slate-950 dark:text-white">${api.plans.count(_.status == ApiPlanStatus.Published)}</dd>
         |      </div>
         |    </dl>
         |  </div>
         |</aside>""".stripMargin
    baseTemplate(title, prefix, api, doc, ctx)(
      s"""<section class="pb-14 pt-8 lg:pt-10">
         |  <div class="container-xxl grid gap-6 xl:grid-cols-[260px_minmax(0,1fr)_260px]">
         |    ${sidebarHtml}
         |    <article class="min-w-0 overflow-hidden rounded-[28px] border border-slate-200/80 bg-white/85 shadow-sm backdrop-blur dark:border-white/10 dark:bg-slate-900/75">
         |      <div class="${if (noInnerPadding) "" else "px-6 py-6 sm:px-8 sm:py-8 "}prose prose-slate max-w-none dark:prose-invert prose-headings:tracking-tight prose-headings:text-slate-950 dark:prose-headings:text-white prose-p:text-slate-600 dark:prose-p:text-slate-300 prose-a:text-sky-600 dark:prose-a:text-sky-400 prose-code:text-slate-900 dark:prose-code:text-slate-100 prose-pre:bg-slate-950 prose-pre:text-slate-50">
         |        ${content}
         |      </div>
         |    </article>
         |    <div class="hidden xl:block">${railHtml}</div>
         |  </div>
         |</section>
         |""".stripMargin)
  }

  def sidebarCategoryTemplate(item: ApiDocumentationSidebarCategory, prefix: String, index: Int, doc: ApiDocumentation, ctx: NgbBackendCallContext): String = {
    s"""<details class="group rounded-2xl border border-slate-200/80 bg-stone-50/70 p-1 dark:border-white/10 dark:bg-white/5" open>
       |  <summary class="flex cursor-pointer list-none items-center justify-between gap-3 rounded-xl px-3 py-3 text-sm font-semibold text-slate-900 dark:text-slate-100">
       |    <span class="inline-flex items-center gap-2">${renderResourceAsIcon(item.icon, doc)}${item.label}</span>
       |    <i class="bi bi-chevron-right"></i>
       |  </summary>
       |  <div class="space-y-1 px-2 pb-2 pt-1">
       |    ${item.links.map(l => sidebarLinkTemplate(l, prefix, doc, ctx)).mkString("\n")}
       |  </div>
       |</details>""".stripMargin
  }

  def sidebarLinkTemplate(item: ApiDocumentationSidebarLink, prefix: String, doc: ApiDocumentation, ctx: NgbBackendCallContext): String = {
    s"""<a class="flex min-h-[40px] items-center gap-2 rounded-xl px-3 py-2 text-sm ${if(ctx.request.path == s"${prefix}${item.link}") "bg-white font-semibold text-slate-950 shadow-sm dark:bg-slate-800 dark:text-white" else "text-slate-600 transition hover:bg-white hover:text-slate-950 dark:text-slate-300 dark:hover:bg-slate-800 dark:hover:text-white"}" href="${prefix}${item.link}">
       |  <span class="inline-flex items-center justify-center text-slate-400 dark:text-slate-500">${renderResourceAsIcon(item.icon, doc)}</span>
       |  <span>${item.label}</span>
       |</a>""".stripMargin
  }

  def sidebarTemplate(sidebar: ApiDocumentationSidebar, prefix: String, doc: ApiDocumentation, ctx: NgbBackendCallContext): String = {
    sidebar.items.zipWithIndex.map {
      case (item @ ApiDocumentationSidebarCategory(_), idx) => sidebarCategoryTemplate(item, prefix, idx, doc, ctx)
      case (item @ ApiDocumentationSidebarLink(_), idx) => sidebarLinkTemplate(item, prefix, doc, ctx)
    }.mkString("\n")
  }

  def navPathActive(paths: Seq[String], ctx: NgbBackendCallContext, prefix: String): String = {
    val path = ctx.request.path.replaceFirst(prefix, "").toLowerCase()
    if (paths.exists(p => path.startsWith(p))) {
      "bg-white text-slate-950 shadow-sm ring-1 ring-slate-200 dark:bg-slate-800 dark:text-white dark:ring-white/10"
    } else {
      "text-slate-500 hover:bg-white/80 hover:text-slate-950 dark:text-slate-300 dark:hover:bg-slate-800 dark:hover:text-white"
    }
  }

  def apikeyModalComponent(): String = {
    s"""
       |<div class="portal-modal fixed inset-0 z-[120] hidden items-center justify-center bg-slate-950/50 p-4 backdrop-blur-sm" id="apiKeyEditorModal" aria-hidden="true">
       |  <div class="w-full max-w-3xl">
       |    <div class="overflow-hidden rounded-[28px] border border-slate-200/80 bg-white shadow-2xl dark:border-white/10 dark:bg-slate-900">
       |      <div class="flex items-start justify-between gap-4 border-b border-slate-200 px-6 py-5 dark:border-white/10">
       |        <div>
       |          <p class="text-[11px] font-bold uppercase tracking-[0.18em] text-sky-600 dark:text-sky-400">Credentials</p>
       |          <div class="mt-2 flex flex-wrap items-center gap-3">
       |            <h2 id="apiKeyEditorTitle" class="text-xl font-semibold tracking-tight text-slate-950 dark:text-white">Create API key</h2>
       |            <span class="inline-flex items-center rounded-full bg-slate-100 px-3 py-1 text-xs font-semibold uppercase tracking-[0.16em] text-slate-500 dark:bg-white/10 dark:text-slate-300" id="apiKeyEditorModeBadge" hidden>update</span>
       |          </div>
       |        </div>
       |        <button type="button" class="inline-flex h-10 w-10 items-center justify-center rounded-full border border-slate-200 bg-white text-slate-500 shadow-sm transition hover:text-slate-950 dark:border-white/10 dark:bg-slate-800 dark:text-slate-300 dark:hover:text-white" data-modal-close="apiKeyEditorModal" aria-label="Close">
       |          <i class="bi bi-x-lg"></i>
       |        </button>
       |      </div>
       |      <div class="max-h-[calc(100vh-14rem)] overflow-auto px-6 py-6">
       |        <form id="apiKeyEditorForm" class="space-y-5">
       |          <div id="apiKeyEditorClientIdRow" class="space-y-2" hidden>
       |            <label for="apiKeyEditorClientId" class="text-sm font-medium text-slate-700 dark:text-slate-200">Client ID</label>
       |            <input type="text" class="w-full rounded-2xl border border-slate-200 bg-stone-50 px-4 py-3 font-mono text-sm text-slate-900 outline-none dark:border-white/10 dark:bg-slate-800 dark:text-slate-100" id="apiKeyEditorClientId" readonly />
       |          </div>
       |
       |          <div class="space-y-2">
       |            <label for="apiKeyEditorName" class="text-sm font-medium text-slate-700 dark:text-slate-200">Name</label>
       |            <input type="text" class="w-full rounded-2xl border border-slate-200 bg-stone-50 px-4 py-3 text-slate-900 outline-none transition focus:border-sky-400 focus:ring-4 focus:ring-sky-100 dark:border-white/10 dark:bg-slate-800 dark:text-slate-100 dark:focus:ring-sky-500/20" id="apiKeyEditorName" data-autofocus />
       |          </div>
       |
       |          <div class="space-y-2">
       |            <label for="apiKeyEditorDesc" class="text-sm font-medium text-slate-700 dark:text-slate-200">Description</label>
       |            <textarea id="apiKeyEditorDesc" class="min-h-[128px] w-full rounded-2xl border border-slate-200 bg-stone-50 px-4 py-3 text-slate-900 outline-none transition focus:border-sky-400 focus:ring-4 focus:ring-sky-100 dark:border-white/10 dark:bg-slate-800 dark:text-slate-100 dark:focus:ring-sky-500/20" rows="4" spellcheck="false"></textarea>
       |          </div>
       |
       |          <label class="inline-flex items-center gap-3 text-sm font-medium text-slate-700 dark:text-slate-200">
       |            <input type="checkbox" id="apiKeyEditorEnabled" class="h-4 w-4 rounded border-slate-300 text-sky-600 focus:ring-sky-500" />
       |            <span>Enabled</span>
       |          </label>
       |
       |          <div id="apiKeyEditorPlanRow" class="space-y-2">
       |            <label for="apiKeyEditorPlan" class="text-sm font-medium text-slate-700 dark:text-slate-200">Plan</label>
       |            <select id="apiKeyEditorPlan" class="w-full rounded-2xl border border-slate-200 bg-stone-50 px-4 py-3 text-slate-900 outline-none transition focus:border-sky-400 focus:ring-4 focus:ring-sky-100 dark:border-white/10 dark:bg-slate-800 dark:text-slate-100 dark:focus:ring-sky-500/20">
       |              <option value="">Choose a plan</option>
       |            </select>
       |            <p class="text-sm text-slate-500 dark:text-slate-400" id="apiKeyEditorPlanHelp"></p>
       |          </div>
       |
       |          <div id="apiKeyEditorPlanDisplay" class="space-y-2" hidden>
       |            <label for="apiKeyEditorDisplay" class="text-sm font-medium text-slate-700 dark:text-slate-200">Plan</label>
       |            <input type="text" class="w-full rounded-2xl border border-slate-200 bg-stone-50 px-4 py-3 text-slate-900 outline-none dark:border-white/10 dark:bg-slate-800 dark:text-slate-100" id="apiKeyEditorDisplay" readonly />
       |            <p class="text-sm text-slate-500 dark:text-slate-400" id="apiKeyEditorPlanHelp2"></p>
       |          </div>
       |
       |          <div id="apiKeyEditorBearerRow" class="space-y-2" hidden>
       |            <label for="apiKeyEditorBearer" class="text-sm font-medium text-slate-700 dark:text-slate-200">Bearer</label>
       |            <div class="flex flex-wrap gap-2 sm:flex-nowrap">
       |              <input type="password" class="min-w-0 flex-1 rounded-2xl border border-slate-200 bg-stone-50 px-4 py-3 font-mono text-sm text-slate-900 outline-none dark:border-white/10 dark:bg-slate-800 dark:text-slate-100" id="apiKeyEditorBearer" readonly />
       |              <button class="inline-flex items-center justify-center rounded-full border border-slate-200 bg-white px-4 py-2 text-sm font-medium text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" type="button" id="apiKeyEditorToggleBearer">Show</button>
       |              <button class="inline-flex items-center justify-center rounded-full border border-slate-200 bg-white px-4 py-2 text-sm font-medium text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" type="button" id="apiKeyEditorCopyBearer">Copy</button>
       |            </div>
       |          </div>
       |        </form>
       |      </div>
       |      <div class="flex items-center justify-between gap-4 border-t border-slate-200 px-6 py-5 dark:border-white/10">
       |        <div class="min-h-[20px] text-sm text-slate-500 dark:text-slate-400" id="apiKeyEditorMsg"></div>
       |        <div class="flex items-center gap-3">
       |          <button type="button" class="inline-flex items-center justify-center rounded-full border border-slate-200 bg-white px-4 py-2.5 text-sm font-semibold text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" data-modal-close="apiKeyEditorModal">Cancel</button>
       |          <button type="button" class="inline-flex items-center justify-center rounded-full bg-slate-950 px-4 py-2.5 text-sm font-semibold text-white shadow-sm transition hover:-translate-y-0.5 dark:bg-white dark:text-slate-950" id="apiKeyEditorSubmitBtn">Save</button>
       |        </div>
       |      </div>
       |    </div>
       |  </div>
       |</div>
       |""".stripMargin
  }

  def testerComponent(): String = {
    s"""
       |<div class="portal-modal fixed inset-0 z-[120] hidden items-center justify-center bg-slate-950/50 p-4 backdrop-blur-sm" id="apiTesterModal" aria-hidden="true">
       |  <div class="w-full max-w-7xl">
       |    <div class="flex max-h-[calc(100vh-2rem)] flex-col overflow-hidden rounded-[28px] border border-slate-200/80 bg-white shadow-2xl dark:border-white/10 dark:bg-slate-900">
       |      <div class="flex items-start justify-between gap-4 border-b border-slate-200 px-6 py-5 dark:border-white/10">
       |        <div class="min-w-0">
       |          <p class="text-[11px] font-bold uppercase tracking-[0.18em] text-sky-600 dark:text-sky-400">Tester</p>
       |          <div class="mt-2 flex flex-wrap items-center gap-3">
       |            <span class="inline-flex items-center rounded-full bg-stone-100 px-3 py-1 text-xs font-semibold uppercase tracking-[0.16em] text-slate-600 dark:bg-white/10 dark:text-slate-300" id="apiTesterMethod">GET</span>
       |            <input type="text" class="min-w-0 flex-1 rounded-2xl border border-slate-200 bg-stone-50 px-4 py-3 font-mono text-sm text-slate-900 outline-none dark:border-white/10 dark:bg-slate-800 dark:text-slate-100" id="apiTesterUrl" readonly />
       |          </div>
       |          <p class="mt-3 hidden text-sm text-amber-600 dark:text-amber-400" id="apiTesterSecretWarn">Headers or body look sensitive. Double-check before copying or sharing.</p>
       |        </div>
       |        <button type="button" class="inline-flex h-10 w-10 items-center justify-center rounded-full border border-slate-200 bg-white text-slate-500 shadow-sm transition hover:text-slate-950 dark:border-white/10 dark:bg-slate-800 dark:text-slate-300 dark:hover:text-white" data-modal-close="apiTesterModal" aria-label="Close">
       |          <i class="bi bi-x-lg"></i>
       |        </button>
       |      </div>
       |      <div class="min-h-0 flex-1 overflow-auto px-6 py-6">
       |        <div class="grid gap-5 xl:grid-cols-2">
       |          <section class="rounded-[24px] border border-slate-200 bg-stone-50/80 p-5 dark:border-white/10 dark:bg-white/5">
       |            <div class="mb-4 flex items-center justify-between gap-3">
       |              <h3 class="text-base font-semibold tracking-tight text-slate-950 dark:text-white">Request</h3>
       |              <button class="inline-flex items-center justify-center rounded-full border border-slate-200 bg-white px-3 py-2 text-sm font-medium text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" id="apiTesterResetHeaders" type="button">Reset headers</button>
       |            </div>
       |            <div class="space-y-4">
       |              <div class="space-y-2">
       |                <label for="apiTesterApiKeySelect" class="text-sm font-medium text-slate-700 dark:text-slate-200">API key</label>
       |                <select id="apiTesterApiKeySelect" class="w-full rounded-2xl border border-slate-200 bg-white px-4 py-3 text-slate-900 outline-none transition focus:border-sky-400 focus:ring-4 focus:ring-sky-100 dark:border-white/10 dark:bg-slate-800 dark:text-slate-100 dark:focus:ring-sky-500/20">
       |                <option value="">No API key</option>
       |              </select>
       |              </div>
       |              <div class="space-y-2">
       |                <label for="apiTesterHeaders" class="text-sm font-medium text-slate-700 dark:text-slate-200">Request headers (JSON)</label>
       |                <textarea id="apiTesterHeaders" class="min-h-[220px] w-full rounded-2xl border border-slate-200 bg-white px-4 py-3 font-mono text-sm text-slate-900 outline-none transition focus:border-sky-400 focus:ring-4 focus:ring-sky-100 dark:border-white/10 dark:bg-slate-800 dark:text-slate-100 dark:focus:ring-sky-500/20" spellcheck="false" placeholder='{"accept":"application/json"}'></textarea>
       |              </div>
       |              <div class="space-y-2">
       |                <label for="apiTesterBody" class="text-sm font-medium text-slate-700 dark:text-slate-200">Request body (JSON)</label>
       |                <textarea id="apiTesterBody" class="min-h-[220px] w-full rounded-2xl border border-slate-200 bg-white px-4 py-3 font-mono text-sm text-slate-900 outline-none transition focus:border-sky-400 focus:ring-4 focus:ring-sky-100 dark:border-white/10 dark:bg-slate-800 dark:text-slate-100 dark:focus:ring-sky-500/20" spellcheck="false" placeholder='{"name":"Alice"}'></textarea>
       |              </div>
       |            </div>
       |          </section>
       |          <section class="rounded-[24px] border border-slate-200 bg-stone-50/80 p-5 dark:border-white/10 dark:bg-white/5">
       |            <div class="mb-4 flex items-center justify-between gap-3">
       |              <h3 class="text-base font-semibold tracking-tight text-slate-950 dark:text-white">Response</h3>
       |              <button class="inline-flex items-center justify-center rounded-full border border-slate-200 bg-white px-3 py-2 text-sm font-medium text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" id="apiTesterClearResponse" type="button">Clear</button>
       |            </div>
       |            <div class="space-y-4">
       |              <div class="grid gap-3 sm:grid-cols-2">
       |                <div class="rounded-2xl bg-white px-4 py-4 dark:bg-slate-800">
       |                  <span class="text-[11px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:text-slate-400">Status</span>
       |                  <strong id="apiTesterStatus" class="mt-2 block text-sm font-semibold text-slate-950 dark:text-white">-</strong>
       |              </div>
       |                <div class="rounded-2xl bg-white px-4 py-4 dark:bg-slate-800">
       |                  <span class="text-[11px] font-bold uppercase tracking-[0.18em] text-slate-500 dark:text-slate-400">Duration</span>
       |                  <strong id="apiTesterDuration" class="mt-2 block text-sm font-semibold text-slate-950 dark:text-white">-</strong>
       |                </div>
       |              </div>
       |              <div class="space-y-2">
       |                <label for="apiTesterRespHeaders" class="text-sm font-medium text-slate-700 dark:text-slate-200">Response headers</label>
       |                <pre id="apiTesterRespHeaders" class="min-h-[180px] overflow-auto rounded-2xl bg-slate-950 px-4 py-4 font-mono text-sm text-slate-100"></pre>
       |              </div>
       |              <div class="space-y-2">
       |                <label for="apiTesterRespBody" class="text-sm font-medium text-slate-700 dark:text-slate-200">Response body</label>
       |                <pre id="apiTesterRespBody" class="min-h-[280px] overflow-auto rounded-2xl bg-slate-950 px-4 py-4 font-mono text-sm text-slate-100"></pre>
       |              </div>
       |            </div>
       |          </section>
       |        </div>
       |      </div>
       |      <div class="flex items-center justify-between gap-4 border-t border-slate-200 px-6 py-5 dark:border-white/10">
       |        <div class="min-h-[20px] text-sm text-rose-600 dark:text-rose-400" id="apiTesterError"></div>
       |        <div class="flex items-center gap-3">
       |          <button type="button" class="inline-flex items-center justify-center rounded-full border border-slate-200 bg-white px-4 py-2.5 text-sm font-semibold text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-800 dark:text-slate-200" data-modal-close="apiTesterModal">Close</button>
       |          <button type="button" class="inline-flex items-center justify-center rounded-full bg-slate-950 px-4 py-2.5 text-sm font-semibold text-white shadow-sm transition hover:-translate-y-0.5 dark:bg-white dark:text-slate-950" id="apiTesterSendBtn">Send request</button>
       |        </div>
       |      </div>
       |    </div>
       |  </div>
       |</div>
       |""".stripMargin
  }

  def baseTemplate(title: String, prefix: String, api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext)(content: String): String = {
    val userInitial = ctx.user
      .flatMap(u => u.name.trim.headOption.orElse(u.email.trim.headOption))
      .map(_.toUpper)
      .getOrElse('U')
    val identityMenu =
      if (ctx.user.isDefined) {
        s"""<details class="relative">
           |  <summary class="flex list-none items-center gap-3 rounded-full border border-slate-200 bg-white px-2 py-1.5 text-sm font-semibold text-slate-700 shadow-sm transition hover:-translate-y-0.5 dark:border-white/10 dark:bg-slate-900 dark:text-slate-200">
           |    <span class="inline-flex h-8 w-8 items-center justify-center rounded-full bg-sky-50 text-xs font-extrabold text-sky-700 dark:bg-sky-500/15 dark:text-sky-300">${userInitial}</span>
           |    <span class="hidden sm:inline">${ctx.user.get.name}</span>
           |    <i class="bi bi-chevron-down"></i>
           |  </summary>
           |  <div class="absolute right-0 top-[calc(100%+0.75rem)] z-50 min-w-[240px] rounded-3xl border border-slate-200 bg-white p-4 shadow-glow dark:border-white/10 dark:bg-slate-900">
           |    <div class="mb-3 border-b border-slate-200 pb-3 dark:border-white/10">
           |      <strong>${ctx.user.get.name}</strong>
           |      <span class="mt-1 block text-sm text-slate-500 dark:text-slate-400">${ctx.user.get.email}</span>
           |    </div>
           |    <a class="text-sm font-semibold text-slate-900 dark:text-white" href="${prefix}/logout">Logout</a>
           |  </div>
           |</details>""".stripMargin
      } else {
        s"""<a class="inline-flex items-center justify-center rounded-full bg-slate-950 px-4 py-2.5 text-sm font-semibold text-white shadow-sm transition hover:-translate-y-0.5 dark:bg-white dark:text-slate-950" href="${prefix}/login?redirect=${prefix}/">Login</a>"""
      }
    val navigationTabs =
      doc.navigation
        .map(nav => s"""<a class="inline-flex items-center gap-2 rounded-full px-4 py-2 text-sm font-medium transition ${navPathActive(nav.path, ctx, prefix)}" href="${prefix}${nav.path.headOption.getOrElse("#")}">${renderResourceAsIcon(nav.icon, doc, Some("max-height: 18px;"))}<span>${nav.label}</span></a>""")
        .mkString("\n")
    val subscriptionsTab =
      if (ctx.user.isDefined && api.plans.exists(_.accessModeConfiguration.map(_.apiKind).getOrElse(ApiKind.Keyless) != ApiKind.Keyless)) {
        s"""<a class="inline-flex items-center gap-2 rounded-full px-4 py-2 text-sm font-medium transition ${navPathActive(Seq("/subscriptions"), ctx, prefix)}" href="${prefix}/subscriptions"><i class="bi bi-key"></i><span>Subscriptions</span></a>"""
      } else {
        ""
      }
    s"""
       |<!doctype html>
       |<html lang="en">
       |  <head>
       |    <meta charset="utf-8" />
       |    <meta name="viewport" content="width=device-width, initial-scale=1" />
       |    <title>${title}</title>
       |    <link rel="icon" href="${prefix}${doc.logo.path.headOption.getOrElse("#")}">
       |    <link rel="preconnect" href="https://fonts.googleapis.com">
       |    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
       |    <script src="https://cdn.jsdelivr.net/npm/@scalar/api-reference"></script>
       |    <link href="https://fonts.googleapis.com/css2?family=Manrope:wght@400;500;600;700;800&family=IBM+Plex+Mono:wght@400;500&display=swap" rel="stylesheet">
       |    <script src="https://cdn.tailwindcss.com?plugins=forms,typography"></script>       |
       |    <script>
       |      tailwind.config = {
       |        darkMode: 'class',
       |        theme: {
       |          extend: {
       |            fontFamily: {
       |              sans: ['Manrope', 'ui-sans-serif', 'system-ui', 'sans-serif'],
       |              mono: ['IBM Plex Mono', 'ui-monospace', 'SFMono-Regular', 'monospace']
       |            },
       |            boxShadow: {
       |              soft: '0 12px 40px rgba(15, 23, 42, 0.08)',
       |              glow: '0 20px 60px rgba(15, 23, 42, 0.14)'
       |            },
       |            colors: {
       |              portal: {
       |                sand: '#f7f4ee',
       |                ink: '#0f172a'
       |              }
       |            }
       |          }
       |        }
       |      };
       |    </script>
       |    <link  href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.css" rel="stylesheet" type="text/css" />
       |    <style type="text/tailwindcss">
       |      @layer base {
       |        body {
       |          @apply min-h-screen bg-stone-50 font-sans text-slate-900 antialiased dark:bg-[#0b1020] dark:text-slate-100;
       |          background-image:
       |            radial-gradient(circle at top left, rgba(56, 189, 248, 0.10), transparent 28%),
       |            radial-gradient(circle at top right, rgba(245, 158, 11, 0.08), transparent 24%);
       |        }
       |        a {
       |          @apply no-underline;
       |        }
       |        summary::-webkit-details-marker {
       |          display: none;
       |        }
       |      }
       |      @layer components {
       |        .container-xxl {
       |          @apply mx-auto w-[min(1440px,calc(100vw-2rem))] px-0;
       |        }
       |        .portal-shell-card {
       |          @apply rounded-[28px] border border-slate-200/80 bg-white/80 shadow-soft backdrop-blur dark:border-white/10 dark:bg-slate-900/70;
       |        }
       |        .portal-surface {
       |          @apply portal-shell-card p-6 sm:p-7;
       |        }
       |        .portal-page-title {
       |          @apply mt-3 text-4xl font-semibold tracking-tight text-slate-950 sm:text-5xl dark:text-white;
       |        }
       |        .portal-page-subtitle {
       |          @apply mt-3 max-w-3xl text-base leading-7 text-slate-600 dark:text-slate-300;
       |        }
       |        .portal-eyebrow {
       |          @apply inline-flex items-center gap-2 text-[11px] font-bold uppercase tracking-[0.18em] text-sky-600 dark:text-sky-400;
       |        }
       |        .portal-pill {
       |          @apply inline-flex items-center rounded-full px-3 py-1 text-xs font-semibold;
       |        }
       |        .portal-pill-success {
       |          @apply portal-pill bg-emerald-50 text-emerald-700 dark:bg-emerald-500/10 dark:text-emerald-300;
       |        }
       |        .portal-pill-warning {
       |          @apply portal-pill bg-amber-50 text-amber-700 dark:bg-amber-500/10 dark:text-amber-300;
       |        }
       |        .portal-pill-muted {
       |          @apply portal-pill bg-slate-100 text-slate-600 dark:bg-white/10 dark:text-slate-300;
       |        }
       |        .portal-feedback {
       |          @apply min-h-[20px] text-sm;
       |        }
       |        .portal-feedback-muted {
       |          @apply text-slate-500 dark:text-slate-400;
       |        }
       |        .portal-feedback-success {
       |          @apply text-emerald-600 dark:text-emerald-400;
       |        }
       |        .portal-feedback-warning {
       |          @apply text-amber-600 dark:text-amber-400;
       |        }
       |        .portal-feedback-danger {
       |          @apply text-rose-600 dark:text-rose-400;
       |        }
       |        .portal-redoc-action {
       |          @apply inline-flex items-center justify-center rounded-full bg-slate-950 px-3 py-1.5 text-xs font-semibold text-white shadow-sm dark:bg-white dark:text-slate-950;
       |        }
       |      }
       |      redoc {
       |        display: block;
       |      }
       |      .portal-redoc-shell > div,
       |      .portal-redoc-shell redoc {
       |        min-height: 70vh;
       |      }
       |      .portal-rich-content .container-xxl {
       |        width: 100%;
       |        max-width: 100%;
       |      }
       |      .portal-rich-content img {
       |        border-radius: 1rem;
       |      }
       |      .portal-rich-content table {
       |        width: 100%;
       |        border-collapse: collapse;
       |      }
       |      .portal-rich-content table th,
       |      .portal-rich-content table td {
       |        border-bottom: 1px solid rgba(148, 163, 184, 0.2);
       |        padding: 0.85rem;
       |        text-align: left;
       |      }
       |      .portal-rich-content .card {
       |        border: 1px solid rgba(148, 163, 184, 0.2);
       |        border-radius: 1.5rem;
       |        background: rgba(255,255,255,0.75);
       |      }
       |      .dark .portal-rich-content .card {
       |        background: rgba(15, 23, 42, 0.72);
       |        border-color: rgba(255,255,255,0.08);
       |      }
       |      .portal-redoc-action {
       |        margin-bottom: 10px;
       |      }
       |    </style>
       |  </head>
       |  <body data-portal-prefix="${prefix}">
       |    <div class="relative isolate">
       |      <header class="sticky top-0 z-50 border-b border-slate-200/70 bg-stone-50/85 backdrop-blur-xl dark:border-white/10 dark:bg-[#0b1020]/80">
       |        <div class="container-xxl flex flex-col gap-4 py-4 lg:py-5">
       |          <div class="flex items-center justify-between gap-4">
       |            <a class="inline-flex min-w-0 items-center gap-3" href="${prefix}/">
       |              <span class="inline-flex h-11 w-11 items-center justify-center rounded-2xl border border-slate-200 bg-white shadow-sm dark:border-white/10 dark:bg-slate-900">
       |                ${renderResourceAsIcon(doc.logo.some, doc, Some("max-height: 26px;"))}
       |              </span>
       |              <span class="truncate text-base font-extrabold tracking-tight text-slate-950 dark:text-white">${api.name}</span>
       |            </a>
       |
       |            <div class="flex items-center gap-3">
       |              <label class="hidden min-w-[18rem] items-center gap-3 rounded-full border border-slate-200 bg-white px-4 py-2.5 text-sm text-slate-500 shadow-sm lg:flex dark:border-white/10 dark:bg-slate-900 dark:text-slate-400">
       |                <i class="bi bi-search"></i>
       |                <input type="search" placeholder="Search docs" class="w-full border-0 bg-transparent p-0 text-slate-900 outline-none ring-0 placeholder:text-slate-400 focus:ring-0 dark:text-slate-100 dark:placeholder:text-slate-500" />
       |              </label>
       |              <label class="inline-flex items-center gap-3 rounded-full border border-slate-200 bg-white px-4 py-2.5 text-sm font-medium text-slate-600 shadow-sm dark:border-white/10 dark:bg-slate-900 dark:text-slate-300">
       |                <i class="bi bi-circle-half"></i>
       |                <select id="themeSelect" class="border-0 bg-transparent p-0 pr-7 text-sm font-medium text-slate-700 outline-none ring-0 focus:ring-0 dark:text-slate-200">
       |                  <option value="system">System</option>
       |                  <option value="light">Light</option>
       |                  <option value="dark">Dark</option>
       |                </select>
       |              </label>
       |              ${identityMenu}
       |            </div>
       |          </div>
       |
       |          <nav class="flex gap-2 overflow-x-auto pb-1" aria-label="Primary">
       |            ${navigationTabs}
       |            <a class="inline-flex items-center gap-2 rounded-full px-4 py-2 text-sm font-medium transition ${navPathActive(Seq("/api-references"), ctx, prefix)}" href="${prefix}/api-references"><i class="bi bi-braces"></i><span>API reference</span></a>
       |            ${subscriptionsTab}
       |          </nav>
       |        </div>
       |      </header>
       |
       |      <main>
       |        ${content}
       |      </main>
       |    </div>
       |    ${apikeyModalComponent()}
       |    ${testerComponent()}
       |    <script src="https://cdn.redoc.ly/redoc/v2.5.1/bundles/redoc.standalone.js"></script>
       |    <script type="text/javascript" src="${prefix}/portal.js"></script>
       |    <script type="module" src="https://cdn.jsdelivr.net/npm/zero-md@3?register"></script>
       |  </body>
       |</html>
       |""".stripMargin
  }

}
