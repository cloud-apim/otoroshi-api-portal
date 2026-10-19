// The developer portal of an Otoroshi API (or a catalog of them), in the browser: its pages rendered from their markdown
// (marked and highlight.js, loaded by the pages that need them), their table of contents, the copy of the code, the
// light and dark themes, the keys of the developer, the API reference (Scalar) with its requests sent through the
// portal, the search of the catalog.

(() => {
  const root = document.documentElement;
  const data = JSON.parse(document.getElementById('portal-data')?.textContent || '{}');
  const prefix = data.prefix || '';
  const isDark = () => root.getAttribute('data-theme') === 'dark';
  const escapeHtml = (value) => String(value ?? '').replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);
  // signing in: back on this page once done
  const loginUrl = (session) => `${session.login_url}?redirect=${encodeURIComponent(location.pathname)}`;
  let reference = null;

  // the key put in the reference of an API (its token, its id), for this tab only
  const tokenKey = (at) => `otoroshi-api-portal-token${at}`;
  const keyIdKey = (at) => `otoroshi-api-portal-key-id${at}`;
  const remembered = (name) => {
    try {
      return sessionStorage.getItem(name);
    } catch (e) {
      return null;
    }
  };
  const remember = (name, value) => {
    try {
      if (value) sessionStorage.setItem(name, value);
      else sessionStorage.removeItem(name);
    } catch (e) {
      // (a private window: for this page only)
    }
  };

  // what the pages ask the portal (`at` its place, then api/…): the writes carry X-Otoroshi-Api-Portal, never sent by a
  // foreign form
  async function callAt(at, method, path, body) {
    const res = await fetch(at + path, {
      method,
      credentials: 'same-origin',
      headers: { Accept: 'application/json', ...(method === 'GET' ? {} : { 'X-Otoroshi-Api-Portal': '1' }), ...(body ? { 'Content-Type': 'application/json' } : {}) },
      ...(body ? { body: JSON.stringify(body) } : {}),
    });
    const json = res.status === 204 ? null : await res.json().catch(() => null);
    if (!res.ok) throw Object.assign(new Error(json?.error_description || json?.error || `the portal answered ${res.status}`), { status: res.status });
    return json;
  }
  const call = (method, path, body) => callAt(prefix, method, path, body);

  /* ---------- the markdown ---------- */

  const slugOf = (text) =>
    String(text)
      .toLowerCase()
      .replace(/<[^>]+>/g, '')
      .replace(/&[a-z0-9#]+;/g, '')
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-+|-+$/g, '') || 'section';

  const codeBlock = (code, lang) => {
    const hljs = window.hljs;
    const language = String(lang || '').split(/\s/)[0].toLowerCase();
    const known = language && hljs && hljs.getLanguage(language);
    const body = known ? hljs.highlight(code, { language, ignoreIllegals: true }).value : escapeHtml(code);
    return `<div class="code"><div class="code-head"><span>${escapeHtml(known ? language : language || 'text')}</span></div><pre><code class="hljs${known ? ` language-${escapeHtml(language)}` : ''}">${body}</code></pre></div>`;
  };

  function renderMarkdown(text) {
    if (!window.marked) return `<pre>${escapeHtml(text)}</pre>`;
    if (window.hljs) window.hljs.registerAliases(['curl', 'console'], { languageName: 'bash' });
    const used = new Map();
    const marked = new window.marked.Marked({
      gfm: true,
      renderer: {
        link({ href, title, tokens }) {
          const external = /^https?:/i.test(href || '');
          return `<a href="${escapeHtml(href)}"${title ? ` title="${escapeHtml(title)}"` : ''}${external ? ' target="_blank" rel="noopener noreferrer"' : ''}>${this.parser.parseInline(tokens)}</a>`;
        },
        code({ text: code, lang }) {
          return codeBlock(code, lang);
        },
        heading({ tokens, depth }) {
          const inner = this.parser.parseInline(tokens);
          const base = slugOf(inner);
          const n = (used.get(base) || 0) + 1;
          used.set(base, n);
          const id = n > 1 ? `${base}-${n}` : base;
          return `<h${depth} id="${id}">${inner}${depth > 1 ? `<a class="anchor" href="#${id}" aria-label="Link to this section">#</a>` : ''}</h${depth}>`;
        },
        table(token) {
          // (wrapped: a wide table scrolls, not the page)
          const head = token.header.map((c) => `<th${c.align ? ` style="text-align:${c.align}"` : ''}>${this.parser.parseInline(c.tokens)}</th>`).join('');
          const rows = token.rows.map((r) => `<tr>${r.map((c) => `<td${c.align ? ` style="text-align:${c.align}"` : ''}>${this.parser.parseInline(c.tokens)}</td>`).join('')}</tr>`).join('');
          return `<div class="table"><table><thead><tr>${head}</tr></thead><tbody>${rows}</tbody></table></div>`;
        },
      },
    });
    return marked.parse(String(text || ''));
  }

  /* ---------- the page ---------- */

  const content = document.getElementById('content');
  if (content) {
    if (typeof data.markdown === 'string') {
      (content.querySelector('.portal-md') || content).innerHTML = renderMarkdown(data.markdown);
    }
    // the paths of the documentation (/docs/…): under the portal, when it is not at the root of its domain
    if (prefix) {
      content.querySelectorAll('a[href], img[src]').forEach((el) => {
        const attribute = el.tagName === 'IMG' ? 'src' : 'href';
        const value = el.getAttribute(attribute) || '';
        if (value.startsWith('/') && !value.startsWith('//') && value !== prefix && !value.startsWith(`${prefix}/`)) el.setAttribute(attribute, prefix + value);
      });
    }
    // the pages written in HTML: their code highlighted, their headings given an id
    if (window.hljs) content.querySelectorAll('pre > code:not(.hljs)').forEach((el) => window.hljs.highlightElement(el));
    const headings = [...content.querySelectorAll('h2, h3')].filter((h) => !h.closest('.code'));
    const used = new Set(headings.map((h) => h.id).filter(Boolean));
    headings.forEach((h) => {
      if (h.id) return;
      let id = slugOf(h.innerHTML);
      for (let n = 2; used.has(id); n++) id = `${slugOf(h.innerHTML)}-${n}`;
      used.add(id);
      h.id = id;
      h.insertAdjacentHTML('beforeend', `<a class="anchor" href="#${id}" aria-label="Link to this section">#</a>`);
    });
    const nav = document.getElementById('toc');
    if (nav) {
      if (headings.length) {
        const textOf = (h) => escapeHtml([...h.childNodes].filter((n) => !(n.classList && n.classList.contains('anchor'))).map((n) => n.textContent).join('').trim());
        nav.innerHTML = headings.map((h) => `<a href="#${h.id}" class="depth-${h.tagName.substring(1)}" data-id="${h.id}">${textOf(h)}</a>`).join('');
        // the section being read, lit
        const links = new Map([...nav.querySelectorAll('a')].map((a) => [a.dataset.id, a]));
        const observer = new IntersectionObserver(
          (entries) => {
            for (const e of entries) {
              if (!e.isIntersecting) continue;
              links.forEach((a) => a.classList.remove('active'));
              links.get(e.target.id)?.classList.add('active');
            }
          },
          { rootMargin: '-80px 0px -70% 0px' }
        );
        headings.forEach((h) => observer.observe(h));
      } else nav.closest('.toc')?.classList.add('empty');
    }
    if (location.hash) document.getElementById(decodeURIComponent(location.hash.slice(1)))?.scrollIntoView();
  }

  // the code copied in a click
  document.querySelectorAll('.code').forEach((block) => {
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'copy';
    button.textContent = 'Copy';
    button.addEventListener('click', () => {
      navigator.clipboard?.writeText(block.querySelector('code')?.innerText || '').then(() => {
        button.textContent = 'Copied';
        setTimeout(() => (button.textContent = 'Copy'), 1200);
      });
    });
    block.querySelector('.code-head')?.appendChild(button);
  });

  /* ---------- the theme ---------- */

  document.getElementById('theme-toggle')?.addEventListener('click', () => {
    const next = isDark() ? 'light' : 'dark';
    root.setAttribute('data-theme', next);
    try {
      localStorage.setItem('otoroshi-api-portal-theme', next);
    } catch (e) {
      // (a private window: for this page only)
    }
    // (the reference: its own mode, the classes of the body)
    document.body.classList.toggle('dark-mode', next === 'dark');
    document.body.classList.toggle('light-mode', next !== 'dark');
    if (reference) reference.updateConfiguration({ ...reference.getConfiguration(), darkMode: next === 'dark', forceDarkModeState: next });
  });

  // the menu of a small screen
  document.getElementById('menu-toggle')?.addEventListener('click', () => document.body.classList.toggle('menu-open'));
  // the menu of the user, closed by a click elsewhere
  document.addEventListener('click', (e) => {
    document.querySelectorAll('.user-menu[open]').forEach((menu) => {
      if (!menu.contains(e.target)) menu.removeAttribute('open');
    });
  });

  /* ---------- the keys of the developer ---------- */

  const count = (n) => Number(n).toLocaleString('en');
  const quotaValue = (n) => (n === null || n === undefined ? 'unlimited' : count(n));
  // the variable a key is put in, in the samples: PETSTORE_KEY
  const envOf = (name) => `${String(name || 'API').toUpperCase().replace(/[^A-Z0-9]+/g, '_').replace(/^_+|_+$/g, '') || 'API'}_KEY`;
  const LANGUAGES = { curl: ['bash', 'curl'], javascript: ['javascript', 'JavaScript'], python: ['python', 'Python'] };

  // a first request with a key, in a few languages: to the gateway of the API, the key in a variable
  const samplesOf = (api) => {
    if (!api?.gateway) return null;
    const url = api.gateway + (api.sample || '/');
    const env = envOf(api.name);
    return {
      env,
      curl: `curl "${url}" \\\n  -H "Authorization: Bearer $${env}"`,
      javascript: `const res = await fetch('${url}', {\n  headers: { Authorization: \`Bearer \${process.env.${env}}\` },\n});\nconsole.log(await res.json());`,
      python: `import os\nimport requests\n\nres = requests.get(\n    "${url}",\n    headers={"Authorization": f"Bearer {os.environ['${env}']}"},\n)\nprint(res.json())`,
    };
  };

  // the calls of a key today and this month, against its quotas
  const meter = (label, usage) => {
    if (!usage) return '';
    const percent = usage.limit ? Math.min(100, Math.round((usage.used / usage.limit) * 100)) : 0;
    return `<div class="meter${percent >= 90 ? ' high' : ''}"><span>${label} <strong>${count(usage.used)}</strong>${usage.limit ? ` / ${count(usage.limit)}` : ''}</span>${usage.limit ? `<span class="meter-bar"><i style="width:${percent}%"></i></span>` : ''}</div>`;
  };

  function mountKeys(mount) {
    // (the API of a new key, in a catalog: the one of ?api=<slug>, or the first one)
    let state = { session: null, keys: [], made: null, editing: null, notice: null, error: null, busy: false, chosenApi: new URLSearchParams(location.search).get('api'), sample: 'curl' };

    const render = () => {
      const { session, keys, made, editing, notice, error, busy } = state;
      if (!session) {
        mount.innerHTML = '<p class="muted">Loading…</p>';
        return;
      }
      // the APIs the keys are taken of: the one of the portal, or the ones of the catalog
      const multi = Array.isArray(session.apis);
      const apis = multi
        ? session.apis
        : [{ prefix, slug: null, name: session.name, plans: session.plans || [], per_user: session.per_user, reason: session.reason, terms: session.terms, gateway: session.gateway, sample: session.sample }];
      const offered = apis.flatMap((api) => api.plans.map((plan) => ({ api, plan })));
      const apiOf = (k) => (multi ? apis.find((a) => a.prefix === k.api_prefix) : apis[0]);
      const planOf = (k) => apiOf(k)?.plans.find((p) => p.id === k.plan);
      const planName = (o) => `${multi ? `${o.api.name} · ` : ''}${o.plan.name}`;
      const perUser = multi ? null : apis[0].per_user;
      if (!session.user) {
        mount.innerHTML = `<div class="panel signin">
<h2>Sign in to take your keys</h2>
<p class="muted">Your keys are yours: nobody else sees them here.</p>
${offered.length ? `<div class="chips">${offered.map((o) => `<span class="badge accent">${escapeHtml(planName(o))}${o.plan.validation === 'auto' ? '' : ' · on approval'}</span>`).join('')}</div>` : ''}
<div><a class="button primary" href="${escapeHtml(loginUrl(session))}">Sign in</a></div>
</div>`;
        return;
      }
      const full = perUser && keys.length >= perUser;
      const keyApis = apis.filter((a) => a.plans.length);
      const chosenApi = keyApis.find((a) => a.slug === state.chosenApi) || keyApis[0];

      // the plans of an API to take a key of, each a card; the reason asked, the terms accepted
      const planCard = (o, i, checked) => {
        const q = o.plan.quotas || {};
        const facts = [q.daily != null ? `${quotaValue(q.daily)} a day` : '', q.monthly != null ? `${quotaValue(q.monthly)} a month` : '', q.window != null ? `bursts of ${quotaValue(q.window)}` : ''].filter(Boolean);
        return `<label class="plan-option"><input type="radio" name="plan" value="${i}"${checked ? ' checked' : ''}>
<span class="plan-name">${escapeHtml(o.plan.name)} ${o.plan.validation === 'auto' ? '<span class="badge success">Key at once</span>' : '<span class="badge warning">On approval</span>'}</span>
${o.plan.description ? `<span class="plan-desc">${escapeHtml(o.plan.description)}</span>` : ''}
<span class="plan-quotas">${escapeHtml(facts.length ? facts.join(' · ') : 'No quota')}</span>
</label>`;
      };
      const apiFields = (api) => {
        const plans = offered.map((o, i) => [o, i]).filter(([o]) => o.api === api);
        return `<fieldset class="plan-picker"><legend>Plan</legend><div class="plan-options">${plans.map(([o, i], n) => planCard(o, i, n === 0)).join('')}</div></fieldset>
<label id="reason-field"${api.reason === 'none' ? ' hidden' : ''}><span>Why do you need this key? <span class="muted" id="reason-optional">(optional)</span></span><textarea name="reason" maxlength="1000" placeholder="What you build with it, and for whom"></textarea></label>
${api.terms ? `<label class="check"><input type="checkbox" name="terms" required><span>I accept the <a href="${escapeHtml(api.terms)}" target="_blank" rel="noopener noreferrer">terms of use</a>${multi ? ` of ${escapeHtml(api.name)}` : ''}</span></label>` : ''}`;
      };

      const keyRow = (k) => {
        const waiting = k.status !== 'validated';
        const actions = `${waiting ? '' : `<button type="button" class="button small" data-reveal="${escapeHtml(k.id)}">Show key</button><button type="button" class="button small" data-rotate="${escapeHtml(k.id)}">Rotate</button>`}<button type="button" class="button small" data-edit="${escapeHtml(k.id)}">Edit</button><button type="button" class="button small danger" data-revoke="${escapeHtml(k.id)}">${waiting ? 'Cancel' : 'Revoke'}</button>`;
        const since = k.created_at ? ` · since ${escapeHtml(new Date(k.created_at).toLocaleDateString())}` : '';
        const usage = k.usage ? `<div class="usage">${meter('Today', k.usage.daily)}${meter('This month', k.usage.monthly)}</div>` : '';
        return `<li><div><strong>${escapeHtml(k.name)}</strong>${waiting ? ' <span class="badge warning">Waiting approval</span>' : k.enabled ? '' : ' <span class="badge">Off</span>'}<div class="muted small">${multi ? `${escapeHtml(k.api_name)} · ` : ''}${escapeHtml(planOf(k)?.name || k.plan_name || k.plan)}${since}</div>${k.description ? `<div class="muted small">${escapeHtml(k.description)}</div>` : ''}${usage}</div><div class="row">${actions}</div></li>`;
      };
      const editingKey = keys.find((k) => k.id === editing);
      const samples = made ? samplesOf(apiOf(made)) : null;
      mount.innerHTML = `<div class="who"><span class="muted">Signed in as</span> <strong>${escapeHtml(session.user.email)}</strong> <a href="${escapeHtml(session.logout_url)}">Sign out</a></div>
${error ? `<div class="notice error">${escapeHtml(error)}</div>` : ''}
${notice ? `<div class="notice">${escapeHtml(notice)}</div>` : ''}
${
  made
    ? `<div class="panel made">
<h3>${escapeHtml(made.name)}: your key</h3>
<p class="muted">Keep it secret: <em>Show key</em> shows it again, to you only. Send it in each request, <code>Authorization: Bearer …</code>, or pick it in the API reference.</p>
<div class="token"><input type="password" id="token" value="${escapeHtml(made.bearer)}" readonly autocomplete="off" spellcheck="false" aria-label="The key" data-1p-ignore data-lpignore="true" data-bwignore data-form-type="other"><button type="button" class="button small" id="show-token">Show</button><button type="button" class="button small" id="copy">Copy</button></div>
${
  samples
    ? `<p class="muted small">A first request: put the key in <code>${escapeHtml(samples.env)}</code>, then</p>
<div class="code samples"><div class="code-head"><span class="tabs">${Object.entries(LANGUAGES).map(([id, [, label]]) => `<button type="button" class="tab${id === state.sample ? ' active' : ''}" data-sample="${id}">${label}</button>`).join('')}</span><button type="button" class="copy" id="copy-sample">Copy</button></div><pre><code class="hljs" id="sample"></code></pre></div>`
    : ''
}
<div class="row"><a class="button small primary" href="${escapeHtml(made.api_prefix ?? prefix)}/api-references" id="use">Use it in the reference</a><button type="button" class="button small" id="done">Done</button></div>
</div>`
    : ''
}
${
  editingKey
    ? `<form class="panel edit" id="edit-key">
<h3>Edit ${escapeHtml(editingKey.name)}</h3>
<label>Name<input name="name" required maxlength="80" value="${escapeHtml(editingKey.name)}" autocomplete="off"></label>
<label>Description<textarea name="description" maxlength="500">${escapeHtml(editingKey.description || '')}</textarea></label>
${editingKey.status === 'validated' ? `<label class="check"><input type="checkbox" name="enabled"${editingKey.enabled ? ' checked' : ''}>Enabled: the requests sent with it are accepted</label>` : ''}
<div class="row"><button type="submit" class="button small primary"${busy ? ' disabled' : ''}>Save</button><button type="button" class="button small" id="cancel-edit">Cancel</button></div>
</form>`
    : ''
}
<div class="panel">
<h3>Your keys <span class="muted small">${keys.length}${perUser ? ` of ${perUser}` : ''}</span></h3>
${keys.length ? `<ul class="key-list">${keys.map(keyRow).join('')}</ul>` : '<p class="muted">No key yet.</p>'}
</div>
${
  offered.length === 0
    ? '<div class="panel"><p class="muted">No plan offers keys on this portal for now.</p></div>'
    : full
      ? `<div class="panel"><p class="muted">${perUser} key${perUser > 1 ? 's' : ''} at most: revoke one to take another.</p></div>`
      : `<form class="panel" id="new-key">
<h3>A new key</h3>
<label>Name<input name="name" required maxlength="80" placeholder="My app" autocomplete="off" data-1p-ignore data-lpignore="true" data-bwignore data-form-type="other"></label>
<label>Description<input name="description" maxlength="500" placeholder="What it is for (optional)" autocomplete="off" data-1p-ignore data-lpignore="true" data-bwignore data-form-type="other"></label>
${multi ? `<label>API<select name="api">${keyApis.map((a) => `<option value="${escapeHtml(a.slug)}"${a === chosenApi ? ' selected' : ''}>${escapeHtml(a.name)}</option>`).join('')}</select></label>` : ''}
<div id="api-fields">${apiFields(chosenApi)}</div>
<div><button type="submit" class="button primary" id="take"${busy ? ' disabled' : ''}></button></div>
</form>`
}`;
      // the form says what happens: a key at once, or a request to approve (a reason then asked, when the portal says so)
      const form = mount.querySelector('#new-key');
      const refresh = () => {
        const o = offered[Number(form.elements.plan.value)];
        const mode = o?.api.reason || 'optional';
        const required = mode === 'required' || (mode === 'manual' && o?.plan.validation !== 'auto');
        form.elements.reason.required = required;
        mount.querySelector('#reason-optional').hidden = required;
        mount.querySelector('#take').textContent = busy ? 'One moment…' : o && o.plan.validation !== 'auto' ? 'Request a key' : 'Take a key';
      };
      if (form) {
        refresh();
        form.addEventListener('change', (e) => {
          if (e.target.name === 'plan') refresh();
        });
        // (another API: its plans, its reason and terms; the name, description and reason typed kept)
        form.elements.api?.addEventListener('change', () => {
          const reason = form.elements.reason.value;
          state = { ...state, chosenApi: form.elements.api.value };
          mount.querySelector('#api-fields').innerHTML = apiFields(keyApis.find((a) => a.slug === state.chosenApi));
          form.elements.reason.value = reason;
          refresh();
        });
      }
      // the key hidden until asked
      mount.querySelector('#show-token')?.addEventListener('click', (e) => {
        const input = mount.querySelector('#token');
        input.type = input.type === 'password' ? 'text' : 'password';
        e.target.textContent = input.type === 'password' ? 'Show' : 'Hide';
      });
      mount.querySelector('#copy')?.addEventListener('click', (e) => {
        navigator.clipboard?.writeText(made.bearer).then(() => (e.target.textContent = 'Copied'));
      });
      // the samples of the key, in the language chosen
      const sample = mount.querySelector('#sample');
      if (sample && samples) {
        const show = (id) => {
          state = { ...state, sample: id };
          const [language] = LANGUAGES[id];
          sample.innerHTML = window.hljs ? window.hljs.highlight(samples[id], { language, ignoreIllegals: true }).value : escapeHtml(samples[id]);
          mount.querySelectorAll('[data-sample]').forEach((t) => t.classList.toggle('active', t.dataset.sample === id));
        };
        show(state.sample);
        mount.querySelectorAll('[data-sample]').forEach((t) => t.addEventListener('click', () => show(t.dataset.sample)));
        mount.querySelector('#copy-sample').addEventListener('click', (e) => {
          navigator.clipboard?.writeText(samples[state.sample]).then(() => {
            e.target.textContent = 'Copied';
            setTimeout(() => (e.target.textContent = 'Copy'), 1200);
          });
        });
      }
      mount.querySelector('#use')?.addEventListener('click', () => {
        remember(tokenKey(made.api_prefix ?? prefix), made.bearer);
        remember(keyIdKey(made.api_prefix ?? prefix), made.id);
      });
      mount.querySelector('#done')?.addEventListener('click', () => update({ made: null, notice: null }));
      mount.querySelector('#cancel-edit')?.addEventListener('click', () => update({ editing: null }));
      mount.querySelectorAll('[data-edit]').forEach((b) => b.addEventListener('click', () => update({ editing: b.dataset.edit, made: null, notice: null, error: null })));
      // a key of the developer, and the portal of its API
      const keyOf = (id) => keys.find((k) => k.id === id);
      const at = (k) => k?.api_prefix ?? prefix;
      mount.querySelectorAll('[data-revoke]').forEach((b) =>
        b.addEventListener('click', async () => {
          if (!confirm('Remove this key? The requests sent with it are refused from now on.')) return;
          const k = keyOf(b.dataset.revoke);
          try {
            await callAt(at(k), 'DELETE', `/api/apikeys/${encodeURIComponent(b.dataset.revoke)}`);
            if (remembered(keyIdKey(at(k))) === b.dataset.revoke) {
              remember(keyIdKey(at(k)), null);
              remember(tokenKey(at(k)), null);
            }
            update({ keys: state.keys.filter((k) => k.id !== b.dataset.revoke), made: null, editing: null, notice: null, error: null });
          } catch (err) {
            update({ error: err.message });
          }
        })
      );
      mount.querySelectorAll('[data-reveal]').forEach((b) =>
        b.addEventListener('click', async () => {
          try {
            const k = keyOf(b.dataset.reveal);
            update({ made: { ...k, ...(await callAt(at(k), 'POST', `/api/apikeys/${encodeURIComponent(b.dataset.reveal)}/reveal`)) }, editing: null, notice: null, error: null });
          } catch (err) {
            update({ error: err.message });
          }
        })
      );
      // a new secret: the key shown again, the one of the reference updated
      mount.querySelectorAll('[data-rotate]').forEach((b) =>
        b.addEventListener('click', async () => {
          const k = keyOf(b.dataset.rotate);
          if (!confirm(`Give ${k.name} a new secret? The requests sent with the current one are refused within a few seconds.`)) return;
          try {
            const rotated = await callAt(at(k), 'POST', `/api/apikeys/${encodeURIComponent(k.id)}/rotate`);
            if (remembered(keyIdKey(at(k))) === k.id) remember(tokenKey(at(k)), rotated.bearer);
            update({ made: { ...k, ...rotated }, notice: `${k.name} has a new secret: put it in your apps, the previous one no longer works.`, editing: null, error: null });
          } catch (err) {
            update({ error: err.message });
          }
        })
      );
      mount.querySelector('#edit-key')?.addEventListener('submit', async (e) => {
        e.preventDefault();
        const values = new FormData(e.target);
        update({ busy: true, error: null });
        try {
          const key = await callAt(at(editingKey), 'PUT', `/api/apikeys/${encodeURIComponent(editing)}`, {
            name: String(values.get('name') || '').trim(),
            description: String(values.get('description') || '').trim(),
            ...(editingKey.status === 'validated' ? { enabled: values.get('enabled') === 'on' } : {}),
          });
          update({ busy: false, editing: null, keys: state.keys.map((k) => (k.id === key.id ? { ...k, ...key } : k)) });
        } catch (err) {
          update({ busy: false, error: err.message });
        }
      });
      form?.addEventListener('submit', async (e) => {
        e.preventDefault();
        const values = new FormData(e.target);
        const { api, plan } = offered[Number(values.get('plan'))];
        update({ busy: true, error: null, notice: null });
        try {
          const created = await callAt(api.prefix, 'POST', '/api/apikeys', {
            name: String(values.get('name') || '').trim(),
            description: String(values.get('description') || '').trim(),
            plan: plan.id,
            reason: String(values.get('reason') || '').trim(),
            accept_terms: values.get('terms') === 'on',
          });
          const key = multi ? { ...created, api: api.slug, api_name: api.name, api_prefix: api.prefix } : created;
          const keys = [...state.keys, key];
          if (key.status === 'validated') update({ busy: false, made: key, keys });
          else update({ busy: false, keys, notice: `Your request for a key of ${key.plan_name || key.plan} is sent: it can be used once approved.` });
        } catch (err) {
          update({ busy: false, error: err.message });
        }
      });
    };

    const update = (patch) => {
      state = { ...state, ...patch };
      render();
    };

    (async () => {
      try {
        const session = await call('GET', '/api/session');
        const keys = session.user ? await call('GET', '/api/apikeys') : [];
        update({ session, keys });
      } catch (err) {
        mount.innerHTML = `<div class="notice error">${escapeHtml(err.message)}</div>`;
      }
    })();
  }

  const keys = document.getElementById('keys');
  if (keys) mountKeys(keys);

  /* ---------- the API reference ---------- */

  const authenticationOf = (token) => ({ ...(data.scalar.authentication || {}), securitySchemes: { [data.securityScheme]: { token: token || '' } } });

  const mount = document.getElementById('reference');
  if (mount && data.scalar) {
    const failed = (why) => {
      mount.classList.add('ready');
      mount.innerHTML = `<p class="muted">${escapeHtml(why)}</p>`;
    };
    // the specification first (a missing one said as such), then the reference
    const spec = fetch(data.scalar.url, { headers: { Accept: 'application/json' } }).then((res) => (res.ok ? res.json() : Promise.reject(new Error(`its source answered ${res.status}`))));
    const script = document.createElement('script');
    script.src = data.scalarScript;
    script.onload = () => {
      spec
        .then((content) => {
          const token = data.securityScheme ? remembered(tokenKey(prefix)) : null;
          const { url, ...scalar } = data.scalar;
          const configuration = {
            ...scalar,
            content,
            ...(token ? { authentication: authenticationOf(token) } : {}),
            // the requests tried from the reference go through the portal: no CORS, the hosts of the API only
            proxyUrl: `${location.origin}${prefix}/api/_proxy`,
            darkMode: isDark(),
            forceDarkModeState: isDark() ? 'dark' : 'light',
          };
          reference = window.Scalar.createApiReference(mount, configuration);
          mount.classList.add('ready');
        })
        .catch((err) => failed(`The specification could not be loaded: ${err.message}.`));
    };
    script.onerror = () => failed('The reference could not be loaded.');
    document.body.appendChild(script);
  }

  /* ---------- the keys of the developer, in the reference ---------- */

  // signed in: their keys in a list, the one chosen put in the authentication of the requests of the reference; signed
  // out: where to sign in
  const picker = document.getElementById('key-picker');
  if (picker && data.securityScheme) {
    const use = (id, token) => {
      remember(keyIdKey(prefix), id);
      remember(tokenKey(prefix), token);
      if (reference) reference.updateConfiguration({ ...reference.getConfiguration(), authentication: authenticationOf(token) });
    };
    call('GET', '/api/session')
      .then(async (session) => {
        if (!session.user) {
          // (signed out since: the key of the tab forgotten)
          if (remembered(keyIdKey(prefix)) || remembered(tokenKey(prefix))) use(null, null);
          picker.innerHTML = `<a class="key-picker-link" href="${escapeHtml(loginUrl(session))}">Sign in to use your keys</a>`;
          return;
        }
        const keys = (await call('GET', '/api/apikeys')).filter((k) => k.status === 'validated');
        const chosen = keys.some((k) => k.id === remembered(keyIdKey(prefix))) ? remembered(keyIdKey(prefix)) : '';
        picker.innerHTML = `<label><span>Tester key</span><select id="key-select" title="The key of the requests you try in the reference">
<option value="">None</option>
${keys.map((k) => `<option value="${escapeHtml(k.id)}"${k.id === chosen ? ' selected' : ''}${k.enabled ? '' : ' disabled'}>${escapeHtml(k.name)} · ${escapeHtml(k.plan_name || k.plan)}${k.enabled ? '' : ' (off)'}</option>`).join('')}
${keys.length ? '' : '<option value="" disabled>No key yet: take one in My keys</option>'}
</select></label>`;
        // (a key revoked since: forgotten)
        if (!chosen && remembered(keyIdKey(prefix))) use(null, null);
        picker.querySelector('#key-select').addEventListener('change', async (e) => {
          const id = e.target.value;
          if (!id) return use(null, null);
          try {
            use(id, (await call('POST', `/api/apikeys/${encodeURIComponent(id)}/reveal`)).bearer);
          } catch (err) {
            e.target.value = '';
            use(null, null);
          }
        });
      })
      .catch(() => picker.remove());
  }

  /* ---------- the catalog ---------- */

  // its APIs found by a text, or a tag
  const search = document.getElementById('api-search');
  if (search) {
    const cards = [...document.querySelectorAll('[data-api]')];
    const chips = [...document.querySelectorAll('[data-tag]')];
    const none = document.getElementById('api-none');
    let tag = null;
    const filter = () => {
      const text = search.value.trim().toLowerCase();
      let shown = 0;
      cards.forEach((card) => {
        const visible = (!text || card.dataset.search.includes(text)) && (!tag || card.dataset.tags.split(',').includes(tag));
        card.hidden = !visible;
        if (visible) shown++;
      });
      if (none) none.hidden = shown > 0;
    };
    search.addEventListener('input', filter);
    chips.forEach((chip) =>
      chip.addEventListener('click', () => {
        tag = tag === chip.dataset.tag ? null : chip.dataset.tag;
        chips.forEach((c) => c.classList.toggle('accent', c.dataset.tag === tag));
        filter();
      })
    );
  }
})();
