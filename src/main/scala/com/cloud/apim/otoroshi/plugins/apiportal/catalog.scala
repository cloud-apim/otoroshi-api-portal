package otoroshi_plugins.com.cloud.apim.plugins.apiportal

import org.apache.pekko.stream.Materializer
import otoroshi.env.Env
import otoroshi.models.PrivateAppsUser
import otoroshi.next.models.*
import otoroshi.next.plugins.api.*
import otoroshi.next.proxy.NgProxyEngineError
import otoroshi.utils.syntax.implicits.*
import play.api.libs.json.*
import play.api.mvc.{Result, Results}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.concurrent.{ExecutionContext, Future}
import scala.util.*

/**
 * The config of a catalog: where it is, the APIs it shows (by id, or the ones with some tags and metadata, or every
 * documented API when none is said), and its look.
 */
case class OtoroshiApiPortalCatalogConfig(
    prefix: Option[String] = None,
    apiRefs: Seq[String] = Seq.empty,
    tags: Seq[String] = Seq.empty,
    metadata: Map[String, String] = Map.empty,
    title: Option[String] = None,
    description: Option[String] = None,
    logo: Option[String] = None,
    theme: Option[String] = None,
    toggle: Boolean = true,
    accent: Option[String] = None,
    links: Seq[(String, String)] = Seq.empty,
    footer: Option[String] = None
) extends NgPluginConfig {
  def json: JsValue = OtoroshiApiPortalCatalogConfig.format.writes(this)

  // an API listed by its id, or one of the APIs its tags and metadata select (every one when nothing is said); a private
  // API only when listed
  def selects(api: Api): Boolean = {
    val nothingSaid = apiRefs.isEmpty && tags.isEmpty && metadata.isEmpty
    val selected    = (tags.nonEmpty || metadata.nonEmpty) && tags.forall(api.tags.contains) && metadata.forall { case (k, v) => api.metadata.get(k).contains(v) }
    apiRefs.contains(api.id) || ((nothingSaid || selected) && api.visibility.kind != ApiVisibilityKind.Private)
  }

  // the look of the catalog, and of the portals of its APIs
  def settings: PortalSettings = PortalSettings(
    title = title.map(_.trim).filter(_.nonEmpty).getOrElse("Developer portal"),
    description = description.map(_.trim).filter(_.nonEmpty),
    theme = theme.filter(t => Seq("system", "light", "dark").contains(t)).getOrElse("system"),
    toggle = toggle,
    accent = accent.map(_.trim).filter(_.matches("^#[0-9a-fA-F]{6}$")).map(_.toLowerCase),
    links = links,
    referenceTheme = "default",
    referenceLayout = "modern",
    referenceClient = true,
    referenceModels = true,
    gatewayServer = true,
    keysPerUser = None
  )
}

object OtoroshiApiPortalCatalogConfig {
  val default = OtoroshiApiPortalCatalogConfig()
  val configFlow: Seq[String] = Seq("prefix", "title", "description", "logo", "api_refs", "tags", "metadata", "theme", "toggle", "accent", "links", "footer")
  val configSchema: Option[JsObject] = Some(
    Json.obj(
      "prefix"      -> Json.obj("type" -> "string", "label" -> "Path prefix"),
      "title"       -> Json.obj("type" -> "string", "label" -> "Title", "placeholder" -> "Developer portal"),
      "description" -> Json.obj("type" -> "string", "label" -> "Description"),
      "logo"        -> Json.obj("type" -> "string", "label" -> "Logo URL"),
      "api_refs"    -> Json.obj(
        "type"  -> "array-select",
        "label" -> "APIs",
        "props" -> Json.obj(
          "optionsFrom"        -> "/bo/api/proxy/apis/apis.otoroshi.io/v1/apis",
          "optionsTransformer" -> Json.obj("label" -> "name", "value" -> "id")
        )
      ),
      "tags"        -> Json.obj("type" -> "string", "array" -> true, "label" -> "APIs with the tags"),
      "metadata"    -> Json.obj("type" -> "object", "label" -> "APIs with the metadata"),
      "theme"       -> Json.obj(
        "type"           -> "select",
        "label"          -> "Theme",
        "possibleValues" -> Json.arr(
          Json.obj("label" -> "System", "value" -> "system"),
          Json.obj("label" -> "Light", "value" -> "light"),
          Json.obj("label" -> "Dark", "value" -> "dark")
        )
      ),
      "toggle"      -> Json.obj("type" -> "bool", "label" -> "Light / dark button"),
      "accent"      -> Json.obj("type" -> "string", "label" -> "Accent color", "placeholder" -> "#4f46e5"),
      "links"       -> Json.obj(
        "type"   -> "array",
        "array"  -> true,
        "label"  -> "Links of the top bar",
        "format" -> "form",
        "schema" -> Json.obj(
          "label" -> Json.obj("type" -> "string", "label" -> "Label"),
          "url"   -> Json.obj("type" -> "string", "label" -> "URL")
        ),
        "flow"   -> Json.arr("label", "url")
      ),
      "footer"      -> Json.obj("type" -> "string", "label" -> "Footer")
    )
  )
  given format: Format[OtoroshiApiPortalCatalogConfig] = new Format[OtoroshiApiPortalCatalogConfig] {
    override def reads(json: JsValue): JsResult[OtoroshiApiPortalCatalogConfig] = Try {
      OtoroshiApiPortalCatalogConfig(
        prefix = json.select("prefix").asOptString.filter(_.nonEmpty),
        apiRefs = json.select("api_refs").asOpt[Seq[String]].getOrElse(Seq.empty),
        tags = json.select("tags").asOpt[Seq[String]].getOrElse(Seq.empty).map(_.trim).filter(_.nonEmpty),
        metadata = json.select("metadata").asOpt[Map[String, String]].getOrElse(Map.empty),
        title = json.select("title").asOptString,
        description = json.select("description").asOptString,
        logo = json.select("logo").asOptString.filter(l => l.startsWith("https://") || l.startsWith("http://") || l.startsWith("/")),
        theme = json.select("theme").asOptString,
        toggle = json.select("toggle").asOpt[Boolean].getOrElse(true),
        accent = json.select("accent").asOptString,
        links = json.select("links").asOpt[Seq[JsObject]].getOrElse(Seq.empty).flatMap { o =>
          for {
            label <- o.select("label").asOpt[String]
            url   <- o.select("url").asOpt[String] if url.startsWith("https://") || url.startsWith("http://") || url.startsWith("mailto:") || url.startsWith("/")
          } yield (label, url)
        },
        footer = json.select("footer").asOptString.filter(_.nonEmpty)
      )
    } match {
      case Failure(e) => JsError(e.getMessage)
      case Success(e) => JsSuccess(e)
    }
    override def writes(o: OtoroshiApiPortalCatalogConfig): JsValue = Json.obj(
      "prefix"      -> o.prefix,
      "api_refs"    -> o.apiRefs,
      "tags"        -> o.tags,
      "metadata"    -> o.metadata,
      "title"       -> o.title,
      "description" -> o.description,
      "logo"        -> o.logo,
      "theme"       -> o.theme,
      "toggle"      -> o.toggle,
      "accent"      -> o.accent,
      "links"       -> JsArray(o.links.map { case (label, url) => Json.obj("label" -> label, "url" -> url) }),
      "footer"      -> o.footer
    )
  }
}

/**
 * A developer portal for several APIs: a home listing them, the keys of the developer for all of them, and the portal of
 * each API under /apis/<slug>, the same pages as the portal of a single API (OtoroshiApiPortal).
 */
class OtoroshiApiPortalCatalog extends NgBackendCall {

  override def useDelegates: Boolean                       = false
  override def multiInstance: Boolean                      = true
  override def core: Boolean                               = true
  override def name: String                                = "Otoroshi API Portal Catalog"
  override def description: Option[String]                 =
    "This plugin serves a developer portal for several APIs: their list, the keys of their developers, and the portal of each API.".some
  override def defaultConfigObject: Option[NgPluginConfig] = OtoroshiApiPortalCatalogConfig.default.some
  override def visibility: NgPluginVisibility              = NgPluginVisibility.NgUserLand
  override def categories: Seq[NgPluginCategory]           = Seq(NgPluginCategory.Other, NgPluginCategory.Custom("Cloud APIM"))
  override def steps: Seq[NgStep]                          = Seq(NgStep.CallBackend)
  override def noJsForm: Boolean                           = true
  override def configFlow: Seq[String]                     = OtoroshiApiPortalCatalogConfig.configFlow
  override def configSchema: Option[JsObject]              = OtoroshiApiPortalCatalogConfig.configSchema

  override def callBackend(
      ctx: NgbBackendCallContext,
      delegates: () => Future[Either[NgProxyEngineError, BackendCallResponse]]
  )(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Either[NgProxyEngineError, BackendCallResponse]] = {
    val config = ctx.cachedConfig(internalName)(OtoroshiApiPortalCatalogConfig.format).getOrElse(OtoroshiApiPortalCatalogConfig.default)
    OtoroshiApiPortalCatalog.catalog(config, ctx).flatMap(catalog => OtoroshiApiPortalCatalog.route(catalog)).asBackendCall
  }
}

/** An API of a catalog: where its portal is (/apis/<slug>), its documentation. */
case class CatalogApi(slug: String, api: Api, doc: ApiDocumentation) {
  lazy val settings: PortalSettings = PortalSettings(api, doc)
  lazy val keyPlans: Seq[ApiPlan]   = PortalPlans.withKeys(api)
}

/** What a request of a catalog is about: its config, its APIs, the visitor. */
case class Catalog(config: OtoroshiApiPortalCatalogConfig, apis: Seq[CatalogApi], ctx: NgbBackendCallContext)(using env: Env) {
  val prefix: String          = config.prefix.getOrElse("")
  lazy val rawPath: String    = {
    val p = ctx.request.path
    if (prefix.nonEmpty && p.startsWith(prefix)) p.substring(prefix.length) else p
  }
  lazy val path: String       = rawPath.toLowerCase()
  lazy val settings           = config.settings
  def keysEnabled: Boolean    = apis.exists(_.keyPlans.nonEmpty)
  lazy val site: CatalogSite  = CatalogSite(prefix, settings, config.logo.map(l => if (l.startsWith("/")) s"$prefix$l" else l), config.footer, keysEnabled)
  def apiPrefix(entry: CatalogApi): String = s"$prefix/apis/${entry.slug}"
  // the portal of one of its APIs, in the catalog
  def portal(entry: CatalogApi): Portal =
    Portal(entry.api, entry.doc, ctx, OtoroshiApiPortalConfig(Some(apiPrefix(entry)), Some(entry.api.id)), Some(site))
}

object OtoroshiApiPortalCatalog {

  import PortalPages.*

  def slugify(value: String): String = value.toLowerCase().replaceAll("[^a-z0-9]+", "-").replaceAll("^-+|-+$", "")

  /** The APIs of a catalog, live (published or deprecated) and documented, each with a slug of its own. */
  def catalog(config: OtoroshiApiPortalCatalogConfig, ctx: NgbBackendCallContext)(using env: Env, ec: ExecutionContext): Future[Catalog] = {
    val selected = env.proxyState
      .allApis()
      .map(OtoroshiApiPortal.withExample)
      .filter(api => (api.state == ApiPublished || api.state == ApiDeprecated) && api.documentation.isDefined && config.selects(api))
      .sortBy(api => (api.name.toLowerCase(), api.id))
    Future
      .sequence(selected.map(api => DocumentationCache.resolve(api).map(_.filter(_.enabled).map(doc => (api, doc))).recover { case _ => None }))
      .map { found =>
        // (a slug taken: the next one gets a number)
        val apis = found.flatten.foldLeft(Seq.empty[CatalogApi]) { case (acc, (api, doc)) =>
          val wanted = doc.metadata.get("portal.slug").orElse(api.metadata.get("portal.slug")).map(slugify).filter(_.nonEmpty)
            .orElse(Some(slugify(api.name)).filter(_.nonEmpty))
            .getOrElse(slugify(api.id))
          val slug   = LazyList.from(1).map(n => if (n == 1) wanted else s"$wanted-$n").find(s => !acc.exists(_.slug == s)).get
          acc :+ CatalogApi(slug, api, doc)
        }
        Catalog(config, apis, ctx)
      }
  }

  def route(catalog: Catalog)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    val method = catalog.ctx.request.method.toLowerCase()
    catalog.path match {
      case "/apis" | "/apis/" => Results.Redirect(s"${catalog.prefix}/", 303).vfuture
      case path if path.startsWith("/apis/") =>
        val slug = path.stripPrefix("/apis/").takeWhile(_ != '/')
        catalog.apis.find(_.slug == slug) match {
          case Some(entry) => OtoroshiApiPortal.route(catalog.portal(entry))
          case None => Results.NotFound(notFoundPage(catalog)).as("text/html").vfuture
        }
      case path => (method, path) match {
        case ("get", p) if p.startsWith("/_portal/") => OtoroshiApiPortal.serveAsset(p.stripPrefix("/_portal/"))
        case ("get", "/login") => OtoroshiApiPortal.redirectAfterLogin(catalog.ctx, s"${catalog.prefix}/")
        case ("get", "/logout") => Results.Redirect("/.well-known/otoroshi/logout", 303).vfuture
        case ("get", "/" | "") => Results.Ok(homePage(catalog)).as("text/html").vfuture
        case ("get", "/keys") if catalog.keysEnabled => Results.Ok(keysPage(catalog)).as("text/html").vfuture
        case ("get", "/subscriptions") if catalog.keysEnabled => Results.Redirect(s"${catalog.prefix}/keys", 303).vfuture
        case ("get", "/api/session") => serveSession(catalog)
        case ("get", "/api/apis") => serveApis(catalog)
        case ("get", "/api/apikeys") => catalog.ctx.user match {
          case None => Results.Unauthorized(Json.obj("error" -> "sign in first")).vfuture
          case Some(user) => serveApikeys(catalog, user)
        }
        case ("get", _) => Results.NotFound(notFoundPage(catalog)).as("text/html").vfuture
        case _ => Results.NotFound(Json.obj("error" -> "not found")).vfuture
      }
    }
  }

  /* ---------- the json of the catalog ---------- */

  def serveSession(catalog: Catalog)(using env: Env, ec: ExecutionContext, mat: Materializer): Future[Result] = {
    // the APIs a key is taken of
    Future.sequence(catalog.apis.filter(_.keyPlans.nonEmpty).map { entry =>
      OtoroshiApiPortal.keysInfo(catalog.portal(entry)).map(_ ++ Json.obj("slug" -> entry.slug, "prefix" -> catalog.apiPrefix(entry)))
    }).map { apis =>
      Results.Ok(Json.obj(
        "user" -> catalog.ctx.user.map(u => Json.obj("name" -> u.name, "email" -> u.email)),
        "login_url" -> s"${catalog.prefix}/login",
        "logout_url" -> s"${catalog.prefix}/logout",
        "apis" -> JsArray(apis),
      )).withHeaders("Cache-Control" -> "no-store")
    }
  }

  def serveApis(catalog: Catalog)(using env: Env, ec: ExecutionContext): Future[Result] = {
    Results.Ok(JsArray(catalog.apis.map { entry =>
      Json.obj(
        "slug" -> entry.slug,
        "name" -> entry.settings.title,
        "description" -> entry.settings.description,
        "version" -> entry.api.version,
        "tags" -> entry.api.tags,
        "prefix" -> catalog.apiPrefix(entry),
        "plans" -> JsArray(PortalPlans.shown(entry.api).map(planJson)),
      )
    })).vfuture
  }

  // the keys of the developer, for all the APIs of the catalog
  def serveApikeys(catalog: Catalog, user: PrivateAppsUser)(using env: Env, ec: ExecutionContext): Future[Result] = {
    Future.sequence(catalog.apis.filter(_.keyPlans.nonEmpty).map { entry =>
      OtoroshiApiPortal.userKeys(catalog.portal(entry), user).flatMap(keys => Future.sequence(keys.map { key =>
        OtoroshiApiPortal.withUsage(key).map(_ ++ Json.obj("api" -> entry.slug, "api_name" -> entry.settings.title, "api_prefix" -> catalog.apiPrefix(entry)))
      }))
    }).map(keys => Results.Ok(JsArray(keys.flatten)).withHeaders("Cache-Control" -> "no-store"))
  }

  /* ---------- the pages of the catalog ---------- */

  def chrome(catalog: Catalog, kind: String): PortalChrome = {
    val prefix = catalog.prefix
    PortalChrome(
      rootPrefix = prefix,
      homeHref = s"$prefix/",
      title = catalog.settings.title,
      description = catalog.settings.description,
      look = catalog.settings,
      logoSrc = catalog.site.logoSrc,
      logoHtml = None,
      crumb = None,
      tabs = Seq(("APIs", s"$prefix/", kind == "catalog")) ++ (if (catalog.keysEnabled) Seq(("My keys", s"$prefix/keys", kind == "keys")) else Seq.empty),
      user = catalog.ctx.user,
      keysHref = Some(s"$prefix/keys").filter(_ => catalog.keysEnabled),
      signInHref = Some(s"$prefix/login?redirect=${URLEncoder.encode(s"$prefix${catalog.rawPath}", StandardCharsets.UTF_8)}")
        .filter(_ => catalog.ctx.user.isEmpty && catalog.keysEnabled && kind != "keys"),
      keyPicker = false,
      bannerHtml = None,
      footerHtml = esc(catalog.config.footer.getOrElse(catalog.settings.title)),
      needsIcons = catalog.apis.exists(a => Json.stringify(a.doc.logo.raw).contains("\"bi "))
    )
  }

  // the tags of an API worth showing: not the ones the catalog selects its APIs with (all of them have those)
  def tagsOf(catalog: Catalog, api: Api): Seq[String] = api.tags.filterNot(catalog.config.tags.contains)

  def apiCard(catalog: Catalog, entry: CatalogApi): String = {
    val home      = catalog.apiPrefix(entry)
    val tags      = tagsOf(catalog, entry.api)
    val doc       = entry.doc
    val logo      = doc.logo
    val logoSrc   = logo.path.headOption
      .filter(_ => logo.url.isDefined || logo.raw.select("base64_content").asOpt[String].isDefined)
      .map(p => s"$home$p")
      .orElse(logo.resolveUrl(doc))
    val icon      = logoSrc.map(src => s"""<img src="${esc(src)}" alt="">""").orElse(Some(renderIcon(Some(logo), doc)).filter(_ => logo.css_icon_class.isDefined))
    val plans     = PortalPlans.shown(entry.api)
    val guides    = doc.navigation.map(links(_).size).sum
    val facts     = Seq(
      Some(doc.references.size).filter(_ > 0).map(n => s"$n specification${if (n > 1) "s" else ""}"),
      Some(guides).filter(_ > 0).map(n => s"$n guide${if (n > 1) "s" else ""}"),
      Some("keys").filter(_ => entry.keyPlans.nonEmpty).map(_ => "with a key")
    ).flatten
    val search    = (Seq(entry.settings.title, entry.settings.description.getOrElse("")) ++ entry.api.tags).mkString(" ").toLowerCase()
    s"""<a class="card" href="${esc(home)}/" data-api="${esc(entry.slug)}" data-search="${esc(search)}" data-tags="${esc(tags.mkString(","))}">
       |${icon.map(i => s"""<span class="card-icon">$i</span>""").getOrElse("")}
       |<h3>${esc(entry.settings.title)}${if (entry.api.version.nonEmpty) s""" <span class="badge">v${esc(entry.api.version)}</span>""" else ""}</h3>
       |${entry.settings.description.map(d => s"<p>${esc(d)}</p>").getOrElse("")}
       |${if (facts.nonEmpty) s"""<p class="muted">${esc(facts.mkString(" · "))}</p>""" else ""}
       |${if (plans.nonEmpty) s"""<div class="chips">${plans.map(p => s"""<span class="badge${if (p.accessModeConfigurationType == "keyless") "" else " accent"}">${esc(p.name)}</span>""").mkString}</div>""" else ""}
       |${if (tags.nonEmpty) s"""<span class="tags">${tags.map(t => s"#${esc(t)}").mkString(" ")}</span>""" else ""}
       |<span class="more">Open its portal →</span>
       |</a>""".stripMargin
  }

  def homePage(catalog: Catalog): String = {
    val settings = catalog.settings
    val prefix   = catalog.prefix
    val tags     = catalog.apis.flatMap(a => tagsOf(catalog, a.api)).distinct.sorted
    val body     =
      s"""<section class="hero"><div class="hero-inner">
         |<span class="eyebrow">Developer portal</span>
         |<h1>${esc(settings.title)}</h1>
         |${settings.description.map(d => s"""<p class="lead">${esc(d)}</p>""").getOrElse("")}
         |<div class="actions"><a class="button primary" href="#apis">Browse the APIs ↓</a>${if (catalog.keysEnabled) s"""<a class="button" href="${esc(prefix)}/keys">My keys</a>""" else ""}</div>
         |</div></section>
         |<div class="container" id="apis">
         |<section class="section">
         |<h2 class="section-title">APIs <span class="muted small">${catalog.apis.size}</span></h2>
         |${if (catalog.apis.isEmpty) """<p class="muted">No API in this portal for now.</p>""" else s"""<div class="catalog-tools"><input id="api-search" type="search" placeholder="Search the APIs" autocomplete="off">${if (tags.nonEmpty) s"""<div class="chips">${tags.map(t => s"""<button type="button" class="badge" data-tag="${esc(t)}">#${esc(t)}</button>""").mkString}</div>""" else ""}</div>"""}
         |<div class="cards">${catalog.apis.map(apiCard(catalog, _)).mkString("\n")}</div>
         |<p class="muted" id="api-none" hidden>No API matches.</p>
         |</section>
         |</div>""".stripMargin
    page(chrome(catalog, "catalog"), prefix, "catalog", None, body)
  }

  def keysPage(catalog: Catalog): String = page(
    chrome(catalog, "keys"),
    catalog.prefix,
    "keys",
    Some("My keys"),
    """<div class="container narrow"><h1 class="page-title">My keys</h1><p class="lead-small muted">The keys of the APIs of this portal: one to send in each request, <code>Authorization: Bearer …</code>.</p><div id="keys"><p class="muted">Loading…</p></div></div>""",
    Json.obj("keys" -> true),
    markdown = true
  )

  def notFoundPage(catalog: Catalog): String = page(
    chrome(catalog, "missing"),
    catalog.prefix,
    "missing",
    Some("Not found"),
    s"""<div class="empty-page"><h1>Not found</h1><p class="muted">No such page in this portal.</p><p><a class="button" href="${esc(catalog.prefix)}/">Back home</a></p></div>"""
  )
}
