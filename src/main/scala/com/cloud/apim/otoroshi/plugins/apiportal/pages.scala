package otoroshi_plugins.com.cloud.apim.plugins.apiportal

import org.apache.pekko.util.ByteString
import otoroshi.env.Env
import otoroshi.models.RemainingQuotas
import otoroshi.next.models.*
import otoroshi.next.plugins.api.NgbBackendCallContext
import otoroshi.utils.http.RequestImplicits.EnhancedRequestHeader
import otoroshi.utils.syntax.implicits.*
import play.api.libs.json.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.text.NumberFormat
import java.util.Locale
import scala.util.Try

/**
 * The files of the portal, served by the plugin (src/main/resources/otoroshi-api-portal), and the libraries the pages
 * load from a CDN, at a fixed version: the reference (Scalar), the markdown (marked), its code (highlight.js).
 */
object PortalAssets {
  private def load(name: String): ByteString = {
    val in = getClass.getResourceAsStream(s"/otoroshi-api-portal/$name")
    if (in == null) ByteString.empty
    else
      try ByteString(in.readAllBytes())
      finally in.close()
  }
  lazy val files: Map[String, (ByteString, String)] = Map(
    "portal.js"  -> (load("portal.js"), "application/javascript; charset=utf-8"),
    "theme.js"   -> (load("theme.js"), "application/javascript; charset=utf-8"),
    "portal.css" -> (load("portal.css"), "text/css; charset=utf-8")
  )
  // (in their addresses: a new version of the plugin, new addresses, never an old file from a cache)
  lazy val version: String = Integer.toHexString(files.values.map(_._1.utf8String).mkString.hashCode)
  def url(prefix: String, name: String): String = s"$prefix/_portal/$name?v=$version"

  val scalar         = "https://cdn.jsdelivr.net/npm/@scalar/api-reference@1.73.1/dist/browser/standalone.js"
  val marked         = "https://cdn.jsdelivr.net/npm/marked@18.0.13/lib/marked.umd.js"
  val highlight      = "https://cdn.jsdelivr.net/npm/@highlightjs/cdn-assets@11.11.1/highlight.min.js"
  val font           = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@200..800&display=swap"
  val bootstrapIcons = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.css"
}

/**
 * The settings of the portal, in the metadata of the documentation of the API (all optional):
 *
 *   - `portal.title`, `portal.description`: the name and the pitch of the home (the name and the description of the API)
 *   - `portal.theme`: `system` (default), `light` or `dark`; `portal.toggle`: `false` hides the light/dark button
 *   - `portal.accent`: the color of the portal, like `#4f46e5`
 *   - `portal.links`: links of the top bar, a json array of `{ "label": "...", "url": "https://..." }`
 *   - `portal.reference.theme`, `portal.reference.layout` (`modern`, `classic`), `portal.reference.client`,
 *     `portal.reference.models`: the API reference (Scalar)
 *   - `portal.reference.gateway_server`: `false` to keep only the servers of the specifications, not the one of the
 *     Otoroshi gateway put first
 *   - `portal.keys.per_user`: how many keys a developer can take
 */
case class PortalSettings(
    title: String,
    description: Option[String],
    theme: String,
    toggle: Boolean,
    accent: Option[String],
    links: Seq[(String, String)],
    referenceTheme: String,
    referenceLayout: String,
    referenceClient: Boolean,
    referenceModels: Boolean,
    gatewayServer: Boolean,
    keysPerUser: Option[Int]
) {
  def locked: Boolean = !toggle && theme != "system"
}

object PortalSettings {
  val referenceThemes: Seq[String] = Seq(
    "default", "alternate", "moon", "purple", "solarized", "bluePlanet", "deepSpace", "saturn", "kepler", "mars", "laserwave", "elysiajs", "fastify", "none"
  )
  def apply(api: Api, doc: ApiDocumentation): PortalSettings = {
    def text(key: String): Option[String]             = doc.metadata.get(key).map(_.trim).filter(_.nonEmpty)
    def bool(key: String, default: Boolean): Boolean  = text(key).map(_.toLowerCase) match {
      case Some("true")  => true
      case Some("false") => false
      case _             => default
    }
    def oneOf(key: String, values: Seq[String], default: String): String = text(key).filter(values.contains).getOrElse(default)
    val links = text("portal.links")
      .flatMap(s => Try(Json.parse(s)).toOption)
      .flatMap(_.asOpt[Seq[JsObject]])
      .getOrElse(Seq.empty)
      .flatMap { o =>
        for {
          label <- o.select("label").asOpt[String]
          url   <- o.select("url").asOpt[String] if url.startsWith("https://") || url.startsWith("http://") || url.startsWith("mailto:") || url.startsWith("/")
        } yield (label, url)
      }
    PortalSettings(
      title = text("portal.title").getOrElse(api.name),
      description = text("portal.description").orElse(Option(api.description).map(_.trim).filter(_.nonEmpty)),
      theme = oneOf("portal.theme", Seq("system", "light", "dark"), "system"),
      toggle = bool("portal.toggle", true),
      accent = text("portal.accent").filter(_.matches("^#[0-9a-fA-F]{6}$")).map(_.toLowerCase),
      links = links,
      referenceTheme = oneOf("portal.reference.theme", referenceThemes, "default"),
      referenceLayout = oneOf("portal.reference.layout", Seq("modern", "classic"), "modern"),
      referenceClient = bool("portal.reference.client", true),
      referenceModels = bool("portal.reference.models", true),
      gatewayServer = bool("portal.reference.gateway_server", true),
      keysPerUser = text("portal.keys.per_user").flatMap(_.toIntOption).filter(_ > 0)
    )
  }
}

/** What a request of the portal is about: the API, its documentation, the visitor, the settings. */
case class Portal(api: Api, doc: ApiDocumentation, ctx: NgbBackendCallContext, config: OtoroshiApiPortalConfig)(using env: Env) {
  val prefix: String           = config.prefix.getOrElse("")
  lazy val settings            = PortalSettings(api, doc)
  // the path under the portal, as asked, and in lower case to find what it serves
  lazy val rawPath: String     = {
    val p = ctx.request.path
    if (prefix.nonEmpty && p.startsWith(prefix)) p.substring(prefix.length) else p
  }
  lazy val path: String        = rawPath.toLowerCase()
  def user                     = ctx.user
  // the plans shown on the portal (published, not private), and the ones a developer takes a key of
  lazy val plans: Seq[ApiPlan] = api.plans.filter(p => p.status == ApiPlanStatus.Published && p.visibility.kind != ApiDocumentationPlanVisibilityKind.Private)
  lazy val keyPlans: Seq[ApiPlan] = plans.filter(_.accessModeConfigurationType == "apikey")
  def keysEnabled: Boolean     = keyPlans.nonEmpty
  def loginUrl(back: String): String = s"$prefix/login?redirect=${URLEncoder.encode(back, StandardCharsets.UTF_8)}"
  def logoutUrl: String        = s"$prefix/logout"
  // where the API answers, through the gateway: its domain, with the scheme and the port the portal is read with
  lazy val gatewayUrl: Option[String] = Option(api.domain).map(_.trim.stripSuffix("/")).filter(_.nonEmpty).map { domain =>
    val port = ctx.rawRequest.theHost.split(":").lift(1).filter(_ => !domain.contains(":")).map(p => s":$p").getOrElse("")
    s"${ctx.rawRequest.theProtocol}://$domain$port${Option(api.contextPath).getOrElse("").stripSuffix("/")}"
  }
  // a link of the documentation: under the portal when it is one of its paths
  def href(link: String): String = if (link.startsWith("/") && !link.startsWith("//")) s"$prefix$link" else link
}

object PortalPages {

  def esc(value: String): String = Option(value)
    .getOrElse("")
    .flatMap {
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case '"'  => "&quot;"
      case '\'' => "&#39;"
      case c    => c.toString
    }
  // a json value inside a page: never the end of its script
  def jsonInPage(value: JsValue): String =
    Json.stringify(value).replace("<", "\\u003c").replace("\u2028", "\\u2028").replace("\u2029", "\\u2029")

  private val ICONS = Map(
    "sun"  -> """<svg class="icon-sun" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><circle cx="12" cy="12" r="4"/><path d="M12 2v2M12 20v2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M2 12h2M20 12h2M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4"/></svg>""",
    "moon" -> """<svg class="icon-moon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 12.8A9 9 0 1 1 11.2 3a7 7 0 0 0 9.8 9.8z"/></svg>""",
    "menu" -> """<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><path d="M4 7h16M4 12h16M4 17h16"/></svg>"""
  )

  /* ---------- the content of the documentation ---------- */

  def hasContent(resource: ApiDocumentationResource): Boolean =
    resource.url.isDefined || resource.base64_content.isDefined || resource.json_content.isDefined || resource.text_content.isDefined

  def isMarkdown(resource: ApiDocumentationResource): Boolean =
    resource.transform.contains("markdown") || resource.contentType.startsWith("text/markdown")

  def isReferenceTransform(resource: ApiDocumentationResource): Boolean =
    resource.transform.exists(t => t == "redoc" || t == "scalar")

  /** A page of the documentation: its html in the page, and its markdown rendered by the browser. */
  case class PageContent(html: String, markdown: Option[String])

  def pageContent(resource: ApiDocumentationResource, body: ByteString): PageContent = {
    if (isMarkdown(resource)) {
      val placeholder = """<div class="portal-md"></div>"""
      PageContent(resource.transform_wrapper.map(_.replace("{content}", placeholder)).getOrElse(""), Some(body.utf8String))
    } else {
      PageContent(body.utf8String, None)
    }
  }

  // the first paragraph of a page, in text: what its card says of it
  def excerptOf(resource: ApiDocumentationResource): Option[String] = {
    resource.description.orElse {
      resource.text_content.flatMap { text =>
        val plain =
          if (isMarkdown(resource)) {
            text
              .split("\n\\s*\n")
              .map(_.trim)
              .find(p => p.nonEmpty && !p.matches("(?s)^(#|```|<|\\||!\\[|[-*>] |\\d+\\. ).*"))
              .map(
                _.replaceAll("!\\[[^\\]]*\\]\\([^)]*\\)", "")
                  .replaceAll("\\[([^\\]]*)\\]\\([^)]*\\)", "$1")
                  .replaceAll("[`*_~]", "")
              )
          } else {
            "(?s)<p[^>]*>(.*?)</p>".r.findFirstMatchIn(text).map(_.group(1).replaceAll("<[^>]+>", ""))
          }
        plain.map(_.replaceAll("\\s+", " ").trim).filter(_.nonEmpty)
      }
    }.map(p => if (p.length > 160) s"${p.substring(0, 157)}…" else p)
  }

  def renderIcon(icon: Option[ApiDocumentationResource], doc: ApiDocumentation): String = icon match {
    case Some(r) if r.css_icon_class.isDefined                    => s"""<i class="${esc(r.css_icon_class.get.replace("me-2", "").trim)}"></i>"""
    case Some(r) if r.text_content.exists(_.startsWith("bi "))    => s"""<i class="${esc(r.text_content.get)}"></i>"""
    case Some(r) if r.text_content.isDefined                      => esc(r.text_content.get)
    case Some(r) if r.resolveUrl(doc).isDefined                   => s"""<img src="${esc(r.resolveUrl(doc).get)}" alt="">"""
    case Some(r) if r.raw.select("base64_content").asOpt[String].isDefined =>
      s"""<img src="data:${esc(r.contentType)};base64,${esc(r.raw.select("base64_content").as[String])}" alt="">"""
    case _                                                        => ""
  }

  /* ---------- the plans ---------- */

  def quotaValue(value: Long): JsValue = if (value >= RemainingQuotas.MaxValue) JsNull else JsNumber(value)
  def quotaText(value: Long): String   = if (value >= RemainingQuotas.MaxValue) "unlimited" else NumberFormat.getIntegerInstance(Locale.ENGLISH).format(value)

  def accessOf(plan: ApiPlan): String = plan.accessModeConfigurationType match {
    case "apikey"        => "API key"
    case "keyless"       => "open"
    case "mtls"          => "mTLS"
    case "jwt"           => "JWT"
    case "oauth2-local"  => "OAuth2"
    case "oauth2-remote" => "OAuth2"
    case other           => other
  }

  def planJson(plan: ApiPlan): JsObject = {
    val quota = plan.rateLimiting.map(_.quota)
    Json.obj(
      "id"          -> plan.id,
      "name"        -> plan.name,
      "description" -> plan.raw.select("description").asOpt[String],
      "access"      -> plan.accessModeConfigurationType,
      "validation"  -> (if (plan.validation.isAuto) "auto" else "manual"),
      "quotas"      -> Json.obj(
        "window"  -> quota.map(q => quotaValue(q.window)).getOrElse(JsNull).asValue,
        "daily"   -> quota.map(q => quotaValue(q.daily)).getOrElse(JsNull).asValue,
        "monthly" -> quota.map(q => quotaValue(q.monthly)).getOrElse(JsNull).asValue
      )
    )
  }

  /* ---------- the layout ---------- */

  def layout(
      portal: Portal,
      kind: String,
      pageTitle: Option[String],
      body: String,
      data: JsObject = Json.obj(),
      markdown: Boolean = false
  ): String = {
    val settings = portal.settings
    val doc      = portal.doc
    val prefix   = portal.prefix
    val title    = settings.title
    val logo     = doc.logo
    val logoSrc  = logo.path.headOption
      .filter(_ => logo.url.isDefined || logo.raw.select("base64_content").asOpt[String].isDefined)
      .map(p => s"$prefix$p")
      .orElse(logo.resolveUrl(doc))
    val brandMark = logoSrc match {
      case Some(src)                           => s"""<img src="${esc(src)}" alt="">"""
      case None if logo.css_icon_class.isDefined => s"""<span class="mark">${renderIcon(Some(logo), doc)}</span>"""
      case None                                => s"""<span class="mark">${esc(Some(title.trim.take(1).toUpperCase).filter(_.nonEmpty).getOrElse("A"))}</span>"""
    }
    val navLinks   = doc.navigation.map { nav =>
      val active = nav.path.exists(p => portal.path.startsWith(p.toLowerCase))
      s"""<a href="${esc(portal.href(nav.path.headOption.getOrElse("#")))}"${if (active) " class=\"active\"" else ""}>${esc(nav.label)}</a>"""
    }
    val referenceLink = s"""<a href="${esc(prefix)}/api-references"${if (kind == "reference" || kind == "references") " class=\"active\"" else ""}>API reference</a>"""
    val keysLink      = if (portal.keysEnabled) s"""<a href="${esc(prefix)}/keys"${if (kind == "keys") " class=\"active\"" else ""}>My keys</a>""" else ""
    val userMenu      = portal.user match {
      case Some(user) =>
        val initial = Option(user.name).flatMap(_.trim.headOption).orElse(Option(user.email).flatMap(_.trim.headOption)).map(_.toUpper.toString).getOrElse("U")
        s"""<details class="user-menu"><summary><span class="avatar">${esc(initial)}</span><span class="user-name">${esc(user.name)}</span></summary>
           |<div class="user-menu-panel"><strong>${esc(user.name)}</strong><span class="muted">${esc(user.email)}</span>${if (portal.keysEnabled) s"""<a href="${esc(prefix)}/keys">My keys</a>""" else ""}<a href="${esc(portal.logoutUrl)}">Sign out</a></div></details>""".stripMargin
      case None if portal.keysEnabled && kind != "reference" && kind != "keys" =>
        s"""<a class="signin-link" href="${esc(portal.loginUrl(s"$prefix${portal.rawPath}"))}">Sign in</a>"""
      case None => ""
    }
    val needsIcons  = Json.stringify(doc.json).contains("\"bi ")
    val banner      = doc.banner.flatMap(_.text_content).map { text =>
      val html = if (doc.banner.exists(_.contentType.startsWith("text/html"))) text else esc(text)
      s"""<div class="banner"><div class="banner-inner">$html</div></div>"""
    }
    val footerText  = doc.footer.flatMap(_.text_content).map { text =>
      if (doc.footer.exists(_.contentType.startsWith("text/html"))) text else esc(text)
    }.getOrElse(esc(title))
    s"""<!doctype html>
       |<html lang="en" data-default-theme="${settings.theme}"${if (settings.locked) s""" data-theme="${settings.theme}" data-locked""" else ""}>
       |<head>
       |<meta charset="utf-8">
       |<meta name="viewport" content="width=device-width, initial-scale=1">
       |<title>${esc(pageTitle.map(t => s"$t · $title").getOrElse(title))}</title>
       |${settings.description.map(d => s"""<meta name="description" content="${esc(d)}">""").getOrElse("")}
       |${logoSrc.map(src => s"""<link rel="icon" href="${esc(src)}">""").getOrElse("")}
       |<script src="${PortalAssets.url(prefix, "theme.js")}"></script>
       |<link rel="preconnect" href="https://fonts.googleapis.com">
       |<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
       |<link rel="stylesheet" href="${PortalAssets.font}">
       |${if (needsIcons) s"""<link rel="stylesheet" href="${PortalAssets.bootstrapIcons}">""" else ""}
       |<link rel="stylesheet" href="${PortalAssets.url(prefix, "portal.css")}">
       |${settings.accent.map(a => s"<style>:root{--accent:$a}</style>").getOrElse("")}
       |</head>
       |<body class="page-$kind">
       |${banner.getOrElse("")}
       |<header class="topbar"><div class="topbar-inner">
       |<a class="brand" href="${esc(prefix)}/">$brandMark<span>${esc(title)}</span></a>
       |<nav class="topnav">
       |${navLinks.mkString("\n")}
       |$referenceLink
       |$keysLink
       |</nav>
       |<div class="spacer"></div>
       |<div class="toplinks">${settings.links.map { case (label, url) => s"""<a href="${esc(portal.href(url))}"${if (url.startsWith("http")) " target=\"_blank\" rel=\"noopener noreferrer\"" else ""}>${esc(label)}</a>""" }.mkString}</div>
       |${if (kind == "reference" && portal.keysEnabled) """<div class="key-picker" id="key-picker"></div>""" else ""}
       |${if (settings.toggle) s"""<button class="icon-btn" id="theme-toggle" type="button" aria-label="Light or dark">${ICONS("sun")}${ICONS("moon")}</button>""" else ""}
       |$userMenu
       |<button class="icon-btn menu-btn" id="menu-toggle" type="button" aria-label="Menu">${ICONS("menu")}</button>
       |</div></header>
       |<main>
       |$body
       |</main>
       |${if (kind == "reference") "" else s"""<footer class="footer"><div class="footer-inner"><span>$footerText</span><span>Powered by <a href="https://www.otoroshi.io" target="_blank" rel="noopener noreferrer">Otoroshi</a> &amp; <a href="https://www.cloud-apim.com" target="_blank" rel="noopener noreferrer">Cloud APIM</a></span></div></footer>"""}
       |<script type="application/json" id="portal-data">${jsonInPage(Json.obj("prefix" -> prefix) ++ data)}</script>
       |${if (markdown) s"""<script src="${PortalAssets.marked}" defer></script>\n<script src="${PortalAssets.highlight}" defer></script>""" else ""}
       |<script src="${PortalAssets.url(prefix, "portal.js")}" defer></script>
       |</body>
       |</html>""".stripMargin
  }

  /* ---------- the home ---------- */

  def referenceHref(portal: Portal, ref: ApiDocumentationResourceRef): String =
    if (portal.doc.references.size > 1) s"${portal.prefix}/api-references${ref.link}" else s"${portal.prefix}/api-references"

  def referenceCards(portal: Portal): String = portal.doc.references.map { ref =>
    val icon  = renderIcon(ref.icon, portal.doc)
    val chips = portal.plans.map(p => s"""<span class="badge${if (p.accessModeConfigurationType == "keyless") "" else " accent"}">${esc(p.name)}</span>""")
    s"""<a class="card" href="${esc(referenceHref(portal, ref))}">
       |${if (icon.nonEmpty) s"""<span class="card-icon">$icon</span>""" else ""}
       |<h3>${esc(ref.title)}${if (portal.api.version.nonEmpty && portal.doc.references.size == 1) s""" <span class="badge">v${esc(portal.api.version)}</span>""" else ""}</h3>
       |<p>${esc(ref.description.getOrElse("Its operations, their parameters and their answers, with a client to try them."))}</p>
       |${if (chips.nonEmpty) s"""<div class="chips">${chips.mkString}</div>""" else ""}
       |<span class="more">Open the reference →</span>
       |</a>""".stripMargin
  }.mkString("\n")

  def planCards(portal: Portal): String = portal.plans.map { plan =>
    val quota   = plan.rateLimiting.map(_.quota)
    val withKey = plan.accessModeConfigurationType == "apikey"
    val facts   = Seq(
      quota.filter(_ => withKey).map(q => ("Burst", if (q.window >= RemainingQuotas.MaxValue) "unlimited" else s"${quotaText(q.window)} / window")),
      quota.filter(_ => withKey).map(q => ("Daily", quotaText(q.daily))),
      quota.filter(_ => withKey).map(q => ("Monthly", quotaText(q.monthly))),
      Some(("Keys", if (plan.validation.isAuto) "at once" else "on approval")).filter(_ => withKey)
    ).flatten
    s"""<div class="card">
       |<h3>${esc(plan.name)} <span class="badge${if (withKey) " accent" else ""}">${esc(if (withKey) "with a key" else accessOf(plan))}</span></h3>
       |${plan.raw.select("description").asOpt[String].map(d => s"<p>${esc(d)}</p>").getOrElse("")}
       |${if (facts.nonEmpty) s"""<ul class="facts">${facts.map { case (k, v) => s"<li><span>${esc(k)}</span><span>${esc(v)}</span></li>" }.mkString}</ul>""" else ""}
       |${if (withKey && portal.keysEnabled) s"""<a class="more" href="${esc(portal.prefix)}/keys">Get a key →</a>""" else ""}
       |</div>""".stripMargin
  }.mkString("\n")

  def guideCards(portal: Portal, nav: ApiDocumentationSidebar): String = {
    val resources = portal.doc.resources
    links(nav).map { link =>
      val resource = resources.find(_.path.exists(_.equalsIgnoreCase(link.link)))
      val excerpt  = resource.flatMap(excerptOf)
      s"""<a class="card" href="${esc(portal.href(link.link))}"><h3>${esc(link.label)}</h3>${excerpt.map(e => s"<p>${esc(e)}</p>").getOrElse("")}<span class="more">Read →</span></a>"""
    }.mkString("\n")
  }

  def homeBody(portal: Portal, home: Option[PageContent]): String = {
    val settings   = portal.settings
    val prefix     = portal.prefix
    val firstGuide = portal.doc.navigation.headOption.flatMap(nav => links(nav).headOption.map(_.link).orElse(nav.path.headOption))
    val actions    = firstGuide match {
      case Some(link) => s"""<a class="button primary" href="${esc(portal.href(link))}">Get started →</a><a class="button" href="${esc(prefix)}/api-references">API reference</a>"""
      case None if portal.doc.references.nonEmpty => s"""<a class="button primary" href="${esc(prefix)}/api-references">API reference →</a>${if (portal.keysEnabled) s"""<a class="button" href="${esc(prefix)}/keys">Get a key</a>""" else ""}"""
      case None => if (portal.keysEnabled) s"""<a class="button primary" href="${esc(prefix)}/keys">Get a key →</a>""" else ""
    }
    val guides     = portal.doc.navigation.filter(nav => links(nav).nonEmpty).map { nav =>
      s"""<section class="section"><h2 class="section-title">${esc(nav.label)}</h2><div class="cards">${guideCards(portal, nav)}</div></section>"""
    }
    s"""<section class="hero"><div class="hero-inner">
       |<span class="eyebrow">Developer portal</span>
       |<h1>${esc(settings.title)}</h1>
       |${settings.description.map(d => s"""<p class="lead">${esc(d)}</p>""").getOrElse("")}
       |${if (actions.nonEmpty) s"""<div class="actions">$actions</div>""" else ""}
       |</div></section>
       |<div class="container">
       |${home.map(h => s"""<article class="prose intro" id="content">${h.html}</article>""").getOrElse("")}
       |${if (portal.doc.references.nonEmpty) s"""<section class="section"><h2 class="section-title">${if (portal.doc.references.size > 1) "APIs" else "API"}</h2><div class="cards">${referenceCards(portal)}</div></section>""" else ""}
       |${if (portal.plans.nonEmpty) s"""<section class="section"><h2 class="section-title">Plans</h2><div class="cards">${planCards(portal)}</div></section>""" else ""}
       |${guides.mkString("\n")}
       |</div>""".stripMargin
  }

  /* ---------- the guides ---------- */

  def links(nav: ApiDocumentationSidebar): Seq[ApiDocumentationSidebarLink] = nav.items.flatMap {
    case link @ ApiDocumentationSidebarLink(_)         => Seq(link)
    case category @ ApiDocumentationSidebarCategory(_) => category.links
  }

  def guideBody(portal: Portal, nav: ApiDocumentationSidebar, content: PageContent, current: String): String = {
    val prefix = portal.prefix
    val linkHtml = (link: ApiDocumentationSidebarLink) =>
      s"""<a href="${esc(portal.href(link.link))}"${if (link.link.toLowerCase == current) " class=\"active\"" else ""}>${esc(link.label)}</a>"""
    // the categories, and the links around them, each a group
    val groups = nav.items.foldLeft(Seq.empty[(Option[String], Seq[ApiDocumentationSidebarLink])]) {
      case (acc, category @ ApiDocumentationSidebarCategory(_)) => acc :+ (Some(category.label), category.links)
      case (acc, link @ ApiDocumentationSidebarLink(_))          =>
        acc.lastOption match {
          case Some((None, ls)) => acc.init :+ (None, ls :+ link)
          case _                => acc :+ (None, Seq(link))
        }
    }
    val groupsHtml = groups.zipWithIndex.map { case ((label, ls), idx) =>
      val title = label.orElse(if (idx == 0) Some(nav.label).filter(_.nonEmpty) else None)
      s"""<div class="sidebar-group">${title.map(t => s"""<div class="sidebar-title">${esc(t)}</div>""").getOrElse("")}${ls.map(linkHtml).mkString}</div>"""
    }
    val refs = portal.doc.references
    val apisGroup =
      if (refs.isEmpty) ""
      else
        s"""<div class="sidebar-group"><div class="sidebar-title">${if (refs.size > 1) "APIs" else "API"}</div><a href="${esc(prefix)}/api-references">API reference</a>${if (refs.size > 1) refs.map(r => s"""<a href="${esc(referenceHref(portal, r))}">${esc(r.title)}</a>""").mkString else ""}${if (portal.keysEnabled) s"""<a href="${esc(prefix)}/keys">My keys</a>""" else ""}</div>"""
    val all  = links(nav)
    val at   = all.indexWhere(_.link.toLowerCase == current)
    val prev = if (at > 0) all.lift(at - 1) else None
    val next = if (at >= 0) all.lift(at + 1) else None
    val pager =
      if (prev.isEmpty && next.isEmpty) ""
      else
        s"""<nav class="pager">${prev.map(p => s"""<a href="${esc(portal.href(p.link))}"><span>Previous</span>${esc(p.label)}</a>""").getOrElse("")}${next.map(n => s"""<a class="next" href="${esc(portal.href(n.link))}"><span>Next</span>${esc(n.label)}</a>""").getOrElse("")}</nav>"""
    s"""<div class="docs">
       |<aside class="sidebar">
       |${groupsHtml.mkString("\n")}
       |$apisGroup
       |</aside>
       |<div class="page">
       |<article class="prose" id="content">${content.html}</article>
       |$pager
       |</div>
       |<aside class="toc"><div class="toc-title">On this page</div><nav id="toc"></nav></aside>
       |</div>""".stripMargin
  }

  /* ---------- the reference ---------- */

  def scalarConfig(portal: Portal, specUrl: String, title: String): JsObject = {
    val settings = portal.settings
    val css      = Seq(
      ".scalar-app { --scalar-font: 'Plus Jakarta Sans', ui-sans-serif, system-ui, sans-serif; --scalar-font-code: ui-monospace, 'JetBrains Mono', Menlo, monospace; --scalar-custom-header-height: 64px; }",
      settings.accent.map(a => s".light-mode, .dark-mode { --scalar-color-accent: $a; }").getOrElse(""),
      if (Seq("default", "none").contains(settings.referenceTheme))
        ".dark-mode { --scalar-background-1: #0a0d12; --scalar-background-2: #10151c; --scalar-background-3: #131922; --scalar-border-color: #1f2731; }"
      else ""
    ).filter(_.nonEmpty).mkString("\n")
    Json.obj(
      "url"                  -> specUrl,
      "theme"                -> settings.referenceTheme,
      "layout"               -> settings.referenceLayout,
      "hideClientButton"     -> !settings.referenceClient,
      "hideTestRequestButton" -> !settings.referenceClient,
      "hideModels"           -> !settings.referenceModels,
      "hideDarkModeToggle"   -> true,
      "withDefaultFonts"     -> false,
      "telemetry"            -> false,
      "agent"                -> Json.obj("disabled" -> true),
      "mcp"                  -> Json.obj("disabled" -> true),
      "showDeveloperTools"   -> "never",
      "documentDownloadType" -> "json",
      "persistAuth"          -> false,
      "customCss"            -> css,
      "metaData"             -> Json.obj("title" -> title)
    ) ++ (if (portal.keysEnabled) Json.obj("authentication" -> Json.obj("preferredSecurityScheme" -> OtoroshiApiPortal.securityScheme)) else Json.obj())
  }

  def referencePage(portal: Portal, specLink: String, title: String): String = {
    val specUrl = s"${portal.prefix}/api/openapi?link=${URLEncoder.encode(specLink, StandardCharsets.UTF_8)}"
    layout(
      portal,
      "reference",
      Some(title),
      """<div id="reference" class="reference"></div>""",
      Json.obj(
        "scalar"         -> scalarConfig(portal, specUrl, title),
        "scalarScript"   -> PortalAssets.scalar,
        "securityScheme" -> (if (portal.keysEnabled) JsString(OtoroshiApiPortal.securityScheme) else JsNull)
      )
    )
  }

  def referencesPage(portal: Portal): String = layout(
    portal,
    "references",
    Some("API reference"),
    s"""<div class="container">
       |<h1 class="page-title">API reference</h1>
       |<p class="lead-small muted">The specifications of ${esc(portal.settings.title)}: pick one to read its operations and try them.</p>
       |<div class="cards">${referenceCards(portal)}</div>
       |</div>""".stripMargin
  )

  /* ---------- the keys ---------- */

  def keysPage(portal: Portal): String = layout(
    portal,
    "keys",
    Some("My keys"),
    """<div class="container narrow"><h1 class="page-title">My keys</h1><p class="lead-small muted">The keys of this API: one to send in each request, <code>Authorization: Bearer …</code>.</p><div id="keys"><p class="muted">Loading…</p></div></div>""",
    Json.obj("keys" -> true)
  )

  def notFoundPage(portal: Portal): String = layout(
    portal,
    "missing",
    Some("Not found"),
    s"""<div class="empty-page"><h1>Not found</h1><p class="muted">No such page in this portal.</p><p><a class="button" href="${esc(portal.prefix)}/">Back home</a></p></div>"""
  )
}
