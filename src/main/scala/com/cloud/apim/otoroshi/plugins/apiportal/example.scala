package otoroshi_plugins.com.cloud.apim.plugins.apiportal

import next.models.{ApiDocumentation, ApiDocumentationPlan, ApiDocumentationRedirection, ApiDocumentationResource, ApiDocumentationResourceRef, ApiDocumentationSidebar, ApiDocumentationSource}
import play.api.libs.json.Json
import otoroshi.utils.syntax.implicits._

object ApiDocumentationExample {
  val remote = ApiDocumentation(
    source = Some(ApiDocumentationSource(Json.obj(
      "url" -> "https://gist.githubusercontent.com/mathieuancelin/6cb4575d206a2028836b01b4e76a1518/raw/portal.config.json"
    ))),
  )
  val value = ApiDocumentation(
    references = Seq(
      ApiDocumentationResourceRef(Json.obj(
        "title" -> "Rick & Morty API",
        "link" -> "/openapi-rm.json",
        "icon" -> Json.obj("css_icon_class" -> "bi bi-rocket")
      )),
      ApiDocumentationResourceRef(Json.obj(
        "title" -> "Otoroshi API",
        "link" -> "/openapi-oto.json",
      )),
    ),
    redirections = Seq(
      ApiDocumentationRedirection(Json.obj("from" -> "/foo", "to" -> "/documentation"))
    ),
    home = ApiDocumentationResource(Json.obj(
      "path" -> "/home",
      "content_type" -> "text/html",
      "site_page" -> true,
      "text_content" ->
        """<div class="container-xxl" style="margin-top: 30px;">
          |  <h1>Home of this awesome API!</h1>
          |  <p>
          |    Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.
          |  </p>
          |  <p>
          |    Lorem ipsum dolor sit amet consectetur adipiscing elit. Quisque faucibus ex sapien vitae pellentesque sem placerat. In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad litora torquent per conubia nostra inceptos himenaeos.
          |  </p>
          |  <a href="/foo">Go to doc !</a>
          |</div>""".stripMargin
    )),
    plans = Seq(ApiDocumentationPlan(Json.obj(
      "id" -> "dev",
      "name" -> "Dev",
      "description" -> "An apikey to try the API on prototypes",
      "throttling_quota" -> 100,
      "daily_quota" -> 1000,
      "monthly_quota" -> 1000,
      "tags" -> Json.arr(),
      "metadata" -> Json.obj(
        "env" -> "dev"
      ),
    ))),
    resources = Seq(
      ApiDocumentationResource(Json.obj(
        "title" -> "Rick & Morty API oas",
        "path" -> "/openapi-rm.json",
        "content_type" -> "application/json",
        "url" -> "https://rickandmorty.zuplo.io/openapi.json"
      )),
      ApiDocumentationResource(Json.obj(
        "title" -> "Otoroshi API oas",
        "path" -> "/openapi-oto.json",
        "content_type" -> "application/json",
        "url" -> "https://maif.github.io/otoroshi/manual/code/openapi.json"
      )),
      ApiDocumentationResource(Json.obj(
        "path" -> "/documentation/getting-started",
        "content_type" -> "text/html",
        "site_page" -> true,
        "text_content" -> "<div class=\"container-xxl\"><h1>Getting started !</h1></div>"
      )),
      ApiDocumentationResource(Json.obj(
        "path" -> "/documentation/more-information",
        "content_type" -> "text/markdown",
        "site_page" -> true,
        "transform" -> "markdown",
        "text_content" -> "# More information\n\n- Lorem ipsum\n- Lorem ipsum\n\n```json\n{\"foo\":\"bar\"}\n```\n\n"
      )),
      ApiDocumentationResource(Json.obj(
        "path" -> "/important/getting-started",
        "content_type" -> "text/html",
        "site_page" -> true,
        "text_content" -> "<div class=\"container-xxl\"><h1>Getting started !</h1></div>"
      )),
      ApiDocumentationResource(Json.obj(
        "path" -> "/important/more-information",
        "content_type" -> "text/markdown",
        "site_page" -> true,
        "transform" -> "markdown",
        "url" -> "https://github.com/MAIF/otoroshi/raw/refs/heads/master/manual/src/main/paradox/about.md"
      ))
    ),
    logo = ApiDocumentationResource(Json.obj(
      "url" -> "https://github.com/MAIF/otoroshi/raw/master/resources/otoroshi-logo.png",
      "path" -> "/favicon.png",
      "content_type" -> "image/png",
    )),
    navigation = Seq(
      ApiDocumentationSidebar(Json.obj(
        "label" -> "Documentation",
        "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2"),
        "path" -> "/documentation",
        "items" -> Json.arr(
          Json.obj(
            "label" -> "Information",
            "kind" -> "category",
            "links" -> Json.arr(
              Json.obj(
                "label" -> "Getting started",
                "link" -> "/documentation/getting-started",
                "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2")
              ),
              Json.obj(
                "label" -> "More information",
                "link" -> "/documentation/more-information",
                "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2")
              )
            )
          ),
          Json.obj(
            "label" -> "API",
            "kind" -> "category",
            "links" -> Json.arr(
              Json.obj(
                "label" -> "API Reference",
                "link" -> "/api-references",
                "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2")
              )
            )
          )
        )
      )),
      ApiDocumentationSidebar(Json.obj(
        "label" -> "Other important stuff",
        "icon" -> Json.obj("css_icon_class" -> "bi bi-exclamation-diamond me-2"),
        "path" -> "/important",
        "items" -> Json.arr(
          Json.obj(
            "label" -> "Getting started",
            "link" -> "/important/getting-started",
            "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2")
          ),
          Json.obj(
            "label" -> "More information",
            "link" -> "/important/more-information",
            "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2")
          )
        )
      )),
    )
  )
  val wines = ApiDocumentation(
    references = Seq(
      ApiDocumentationResourceRef(Json.obj(
        "title" -> "Wines API",
        "link" -> "/openapi.json",
      )),
    ),
    home = ApiDocumentationResource(Json.obj(
      "path" -> "/home",
      "content_type" -> "text/html",
      "site_page" -> true,
      "transform" -> "markdown",
      "transform_wrapper" -> """<div class="container-xxl" style="margin-top: 30px;">{content}</div>""",
      "text_content" ->
        """<div class="container-xxl" style="margin-top: 30px;">
          |
          |# 🍷 Welcome to the Wine API
          |
          |Discover the world of wines through data!
          |The **Wine API** gives you access to a rich catalog of wines, grape varieties, wineries, and wine regions from around the world. Whether you’re building an app for wine lovers, a recommendation engine, or an internal dashboard for your cellar — this API provides everything you need.
          |
          |---
          |
          |## 🌍 What You Can Do
          |
          |- **Browse wines** — Retrieve detailed information about wines, including tasting notes, vintage, grape composition, and producer details.
          |- **Explore wine regions** — Discover wine-growing areas, appellations, and terroirs from every continent.
          |- **Collect ratings and reviews** — Allow users to rate wines and leave comments to build community insights.
          |- **Filter and search** — Find wines by region, grape variety, food pairing, or score.
          |
          |---
          |
          |## 🧭 Getting Started
          |
          |1. **Sign up** for an API key on this portal.
          |2. **Check out the documentation** to learn about available endpoints.
          |3. **Start querying** — for example, try:
          |
          |   ```bash
          |   curl https://wines-api-sandbox-01j0vgh9zmnnzdmzn8jxbc40pe.cloud-apim.dev/api/wines?region=Bordeaux
          |   ```
          |
          |to know more about it, go to the [documentation](/documentation)
          |</div>
          |""".stripMargin
    )),
    plans = Seq(ApiDocumentationPlan(Json.obj(
      "id" -> "dev",
      "name" -> "Dev",
      "description" -> "An apikey to try the API on prototypes",
      "throttling_quota" -> 100,
      "daily_quota" -> 1000,
      "monthly_quota" -> 1000,
      "tags" -> Json.arr(),
      "metadata" -> Json.obj(
        "env" -> "dev"
      ),
    ))),
    resources = Seq(
      ApiDocumentationResource(Json.obj(
        "title" -> "Wines API",
        "path" -> "/openapi.json",
        "content_type" -> "application/json",
        "url" -> "https://wines-api-sandbox-01j0vgh9zmnnzdmzn8jxbc40pe.cloud-apim.dev/docs/openapi.json"
      )),
      ApiDocumentationResource(Json.obj(
        "path" -> "/documentation/getting-started",
        "content_type" -> "text/html",
        "site_page" -> true,
        "transform" -> "markdown",
        "url" -> "https://github.com/mathieuancelinserli/wines-api-cloud-apim-serverless/raw/refs/heads/main/docs/index.md"
      )),
      ApiDocumentationResource(Json.obj(
        "path" -> "/documentation/latency",
        "content_type" -> "text/markdown",
        "site_page" -> true,
        "transform" -> "markdown",
        "url" -> "https://github.com/mathieuancelinserli/wines-api-cloud-apim-serverless/raw/refs/heads/main/docs/latency.md"
      )),
      ApiDocumentationResource(Json.obj(
        "path" -> "/docs/winesapp.png",
        "content_type" -> "image/png",
        "url" -> "https://github.com/mathieuancelinserli/wines-api-cloud-apim-serverless/raw/refs/heads/main/docs/winesapp.png"
      )),
      ApiDocumentationResource(Json.obj(
        "path" -> "/docs/defer.png",
        "content_type" -> "image/png",
        "url" -> "https://github.com/mathieuancelinserli/wines-api-cloud-apim-serverless/raw/refs/heads/main/docs/defer.png"
      )),
    ),
    logo = ApiDocumentationResource(Json.obj(
      "url" -> "https://github.com/react-bootcamp/react-workshop/raw/master/step-2-done/public/img/chevrol-bel-air.png",
      "path" -> "/favicon.png",
      "content_type" -> "image/png",
    )),
    navigation = Seq(
      ApiDocumentationSidebar(Json.obj(
        "label" -> "Documentation",
        "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2"),
        "path" -> "/documentation",
        "items" -> Json.arr(
          Json.obj(
            "label" -> "Information",
            "kind" -> "category",
            "links" -> Json.arr(
              Json.obj(
                "label" -> "Getting started",
                "link" -> "/documentation/getting-started",
                "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2")
              ),
              Json.obj(
                "label" -> "Latency",
                "link" -> "/documentation/latency",
                "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2")
              )
            )
          ),
          Json.obj(
            "label" -> "API",
            "kind" -> "category",
            "links" -> Json.arr(
              Json.obj(
                "label" -> "API Reference",
                "link" -> "/api-references",
                "icon" -> Json.obj("css_icon_class" -> "bi bi-journal-text me-2")
              )
            )
          )
        )
      )),
    )
  )
}
