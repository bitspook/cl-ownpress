function preferedColorScheme() {
  return window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark': 'light'
}

function run() {
  document.getRootNode().firstChild.classList.add(preferedColorScheme())
}

run()
