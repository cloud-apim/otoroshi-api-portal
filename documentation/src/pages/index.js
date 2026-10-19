import Link from '@docusaurus/Link';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import CodeBlock from '@theme/CodeBlock';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './index.module.css';

const capabilities = [
  {
    kicker: 'Docs',
    title: 'Guides that read well',
    body: 'Markdown or HTML pages, inline or fetched from a git repository, with a sidebar, an "On this page" table of contents, highlighted code with a copy button, and previous / next links.',
  },
  {
    kicker: 'Reference',
    title: 'A reference you can try',
    body: 'Your OpenAPI specifications rendered with Scalar, JSON or YAML. Requests go through the portal to the Otoroshi gateway: no CORS to open, and only to the hosts of the API.',
  },
  {
    kicker: 'Reference',
    title: 'The gateway, already there',
    body: 'The gateway of the API is put first in the servers of the specification, and the keys of the developers as its security scheme: a developer picks their key in the top bar and calls.',
  },
  {
    kicker: 'Keys',
    title: 'Self-service API keys',
    body: 'Developers sign in with your Otoroshi auth module, pick a plan among cards, and get a key at once — or wait for an approval you confirm in Otoroshi, with the reason they gave.',
  },
  {
    kicker: 'Keys',
    title: 'Everything about a key',
    body: 'Its calls today and this month against its quotas, a secret to rotate, ready-to-paste requests in curl, JavaScript and Python, and the usual rename, disable and revoke.',
  },
  {
    kicker: 'Keys',
    title: 'Terms and reasons',
    body: 'Ask developers to accept your terms of use and to say why they need a key. Both are kept in the metadata of their subscription, where the admin who approves reads them.',
  },
  {
    kicker: 'Catalog',
    title: 'One portal, many APIs',
    body: 'A second plugin lists the APIs you pick by id, tags or metadata, with a search, one sign in and one place for all the keys — and serves the portal of each API under it.',
  },
  {
    kicker: 'Operations',
    title: 'Nothing else to run',
    body: 'The portal is an Otoroshi plugin: no frontend to deploy, no database of its own. It reads the documentation of your APIs and writes Otoroshi subscriptions and API keys.',
  },
  {
    kicker: 'Operations',
    title: 'Configuration as data',
    body: 'Everything is in the documentation section of the API, or in a JSON file loaded from a URL: version your portal next to your API, and the portal follows within seconds.',
  },
];

const quickstart = `# otoroshi, and the portal plugins next to it
curl -L -o otoroshi.jar \\
  'https://github.com/MAIF/otoroshi/releases/download/v18.0.0/otoroshi.jar'
# the portal jar, from github.com/cloud-apim/otoroshi-api-portal/releases/latest
curl -L -o otoroshi-api-portal.jar \\
  '.../releases/download/<version>/otoroshi-api-portal_3-<version>.jar'

java -cp "./otoroshi-api-portal.jar:./otoroshi.jar" play.core.server.ProdServerStart`;

function Hero() {
  return (
    <header className={styles.hero}>
      <div className={styles.grid} />
      <div className={`container ${styles.heroInner}`}>
        <span className={styles.eyebrow}>Cloud APIM · Otoroshi plugins</span>
        <Heading as="h1" className={styles.heroTitle}>
          A developer portal for your <span className={styles.accent}>Otoroshi APIs</span>
        </Heading>
        <p className={styles.heroSubtitle}>
          Documentation, an API reference to try requests from, and self-service API keys — served by
          Otoroshi itself, from the documentation of your APIs. One API per portal, or a catalog of
          them.
        </p>
        <div className={styles.heroButtons}>
          <Link className={styles.buttonPrimary} to="/docs/overview">
            Read the docs →
          </Link>
          <Link className={styles.button} to="/docs/quickstart">
            Quickstart
          </Link>
          <Link className={styles.button} href="https://github.com/cloud-apim/otoroshi-api-portal">
            GitHub
          </Link>
        </div>
        <img className={styles.heroShot} src={useBaseUrl('/img/screenshots/catalog-api.png')} alt="A portal of an API, in a catalog" />
      </div>
    </header>
  );
}

function Capabilities() {
  return (
    <section className={styles.section}>
      <div className="container">
        <div className={styles.sectionTag}>Capabilities</div>
        <Heading as="h2" className={styles.sectionTitle}>
          What your developers get
        </Heading>
        <p className={styles.sectionLede}>
          The look and the features of the developer portals of Cloud APIM Serverless, on any API you
          expose through Otoroshi.
        </p>
        <div className={styles.cards}>
          {capabilities.map((c) => (
            <div className={styles.card} key={c.title}>
              <div className={styles.cardKicker}>{c.kicker}</div>
              <div className={styles.cardTitle}>{c.title}</div>
              <p className={styles.cardBody}>{c.body}</p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}

function Plugins() {
  return (
    <section className={`${styles.section} ${styles.sectionAlt}`}>
      <div className="container">
        <div className={styles.sectionTag}>Two plugins</div>
        <Heading as="h2" className={styles.sectionTitle}>
          One API, or all of them
        </Heading>
        <div className={styles.plugins}>
          <Link className={styles.plugin} to="/docs/portal/setup">
            <img src={useBaseUrl('/img/screenshots/home.png')} alt="The portal of an API" />
            <div className={styles.pluginTitle}>Otoroshi API Portal</div>
            <p>The portal of one API, on its own domain: its home, its guides, its reference, its keys.</p>
          </Link>
          <Link className={styles.plugin} to="/docs/catalog">
            <img src={useBaseUrl('/img/screenshots/catalog.png')} alt="A catalog of APIs" />
            <div className={styles.pluginTitle}>Otoroshi API Portal Catalog</div>
            <p>A portal for several APIs, picked by id, tags or metadata, with the portal of each one inside.</p>
          </Link>
        </div>
      </div>
    </section>
  );
}

function Start() {
  return (
    <section className={styles.section}>
      <div className="container">
        <div className={styles.sectionTag}>Get going</div>
        <Heading as="h2" className={styles.sectionTitle}>
          A jar next to Otoroshi
        </Heading>
        <p className={styles.sectionLede}>
          Download the jar from the releases page, start Otoroshi with it, then add a route with the
          portal plugin in front of an API that has a documentation.
        </p>
        <div className={styles.snippet}>
          <CodeBlock language="bash">{quickstart}</CodeBlock>
        </div>
      </div>
    </section>
  );
}

export default function Home() {
  return (
    <Layout
      title="A developer portal for your Otoroshi APIs"
      description="Documentation, an API reference you can try requests from, and self-service API keys, served by Otoroshi from the documentation of your APIs.">
      <Hero />
      <main>
        <Capabilities />
        <Plugins />
        <Start />
      </main>
    </Layout>
  );
}
