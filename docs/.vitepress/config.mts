import { withMermaid } from 'vitepress-plugin-mermaid'
import apiSidebar from '../api/_sidebar.json'

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
      { text: 'Home', link: '/' },
      {
        text: 'Guide',
        items: [
          { text: 'About',             link: '/guide/' },
          { text: 'Features',          link: '/guide/features' },
          { text: 'Installation',      link: '/guide/installation' },
          { text: 'Usage',             link: '/guide/usage' },
          { text: 'API Reference',     link: '/guide/api-reference' },
          { text: 'Contributing',      link: '/guide/contributing' },
          { text: 'Coverage Analysis', link: '/guide/coverage-analysis' },
          { text: 'Changelog',         link: '/guide/changelog' },
        ],
      },
      { text: 'API', link: '/api/' },
      { text: 'GitHub', link: 'https://github.com/szaghi/VTKFortran' },
    ],
    sidebar: {
      '/guide/': [
        {
          text: 'Introduction',
          items: [
            { text: 'About',    link: '/guide/' },
            { text: 'Features', link: '/guide/features' },
          ],
        },
        {
          text: 'Getting Started',
          items: [
            { text: 'Installation',  link: '/guide/installation' },
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
      ],
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
    optimizeDeps: {
      include: ['mermaid'],
    },
  },
})
