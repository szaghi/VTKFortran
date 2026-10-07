import { withMermaid } from 'vitepress-plugin-mermaid'
import apiSidebar from '../api/_sidebar.json'

// one sidebar for every page but the API, in reading order: the "previous" and "next" links at the bottom of a page
// follow it, so the documentation reads from the first page to the last
const docs = [
  {
    text: 'Start here',
    items: [
      { text: 'Introduction', link: '/guide/' },
      { text: 'Installation', link: '/guide/installation' },
    ],
  },
  {
    text: 'Tutorial',
    items: [
      { text: 'Overview',                link: '/manual/' },
      { text: '1. A first file',         link: '/manual/tutorial/01-first-file' },
      { text: '2. Formats',              link: '/manual/tutorial/02-formats' },
      { text: '3. More data',            link: '/manual/tutorial/03-more-data' },
      { text: '4. A time series',        link: '/manual/tutorial/04-time-series' },
      { text: '5. An unstructured mesh', link: '/manual/tutorial/05-unstructured' },
      { text: '6. Going parallel',       link: '/manual/tutorial/06-parallel' },
      { text: '7. An assembly',          link: '/manual/tutorial/07-assembly' },
      { text: '8. Restart',              link: '/manual/tutorial/08-restart' },
    ],
  },
  {
    text: 'Reference',
    items: [
      { text: 'Features',      link: '/guide/features' },
      { text: 'Usage',         link: '/guide/usage' },
      { text: 'API Reference', link: '/guide/api-reference' },
    ],
  },
  {
    text: 'Project',
    items: [
      { text: 'Contributing',      link: '/guide/contributing' },
      { text: 'Coverage Analysis', link: '/guide/coverage-analysis' },
      { text: 'Changelog',         link: '/guide/changelog' },
    ],
  },
]

export default withMermaid({
  title: 'VTKFortran Documentation',
  base: '/VTKFortran/',
  markdown: {
    math: true,
    languages: ['fortran-free-form', 'fortran-fixed-form'],
    languageAlias: {
      'fortran': 'fortran-free-form',
      'f90': 'fortran-free-form',
      'f95': 'fortran-free-form',
      'f03': 'fortran-free-form',
      'f08': 'fortran-free-form',
      'f77': 'fortran-fixed-form',
    },
  },
  themeConfig: {
    nav: [
      { text: 'Start here', link: '/guide/', activeMatch: '^/guide/(index|installation)' },
      { text: 'Tutorial', link: '/manual/tutorial/01-first-file', activeMatch: '^/manual/(index|tutorial/)' },
      {
        text: 'Reference',
        link: '/guide/features',
        activeMatch: '^/guide/(features|usage|api-reference)',
      },
      { text: 'API', link: '/api/' },
      {
        text: 'Project',
        items: [
          { text: 'Contributing',      link: '/guide/contributing' },
          { text: 'Coverage Analysis', link: '/guide/coverage-analysis' },
          { text: 'Changelog',         link: '/guide/changelog' },
        ],
      },
      { text: 'GitHub', link: 'https://github.com/szaghi/VTKFortran' },
    ],
    sidebar: {
      '/guide/': docs,
      '/manual/': docs,
      '/api/': [
        {
          text: 'API Reference',
          items: [
            { text: 'Overview', link: '/api/' },
          ],
        },
        ...apiSidebar,
      ],
    },
    search: {
      provider: 'local',
    },
  },
  mermaid: {},
  vite: {
    build: {
      target: 'es2022',
    },
    optimizeDeps: {
      include: ['mermaid'],
    },
  },
})
