// The theme of the portal, set before the page is painted (no flash): the one the visitor chose, or the one of the
// portal (the `portal.theme` metadata of its documentation), or the one of their system.
(function () {
  var root = document.documentElement;
  if (root.hasAttribute('data-locked')) return;
  var chosen = null;
  try {
    chosen = localStorage.getItem('otoroshi-api-portal-theme');
  } catch (e) {
    chosen = null;
  }
  var theme = chosen || root.getAttribute('data-default-theme') || 'system';
  if (theme !== 'light' && theme !== 'dark') theme = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
  root.setAttribute('data-theme', theme);
})();
