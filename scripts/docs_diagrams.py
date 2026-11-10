"""Draw the diagrams of the documentation: docs/diagrams/*.svg, inlined in the pages with <!--@include: -->.

Usage: python3 scripts/docs_diagrams.py            the diagrams, docs/diagrams/*.svg
       python3 scripts/docs_diagrams.py --charts   the charts of the example outputs, docs/examples/images/*.svg (run by
                                                   scripts/docs_examples.sh after the examples: their numbers come from
                                                   the outputs)

The diagrams are drawn with the CSS classes of docs/.vitepress/theme/custom.css (`.vtk-diagram`, `d-*`), which take their
colours from the theme tokens: the same SVG reads in the light and in the dark theme. Each SVG is written on lines with
no blank line among them, so that the Markdown include keeps it one HTML block. Regenerate them after changing this
script; never edit the SVG files by hand.
"""

from __future__ import annotations

import math
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / 'docs' / 'diagrams'
EXAMPLES = ROOT / 'docs' / 'examples'


def esc(text: str) -> str:
    """Escape the XML special characters of a text."""
    return text.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;').replace('"', '&quot;')


class Svg:
    """A minimal SVG writer: elements appended in order, one per line."""

    def __init__(self, width: int, height: int, title: str, uid: str) -> None:
        self.uid = uid
        self.lines = [f'<svg class="vtk-diagram" viewBox="0 0 {width} {height}" role="img" aria-labelledby="{uid}-t">',
                      f'<title id="{uid}-t">{esc(title)}</title>']

    def add(self, line: str) -> None:
        self.lines.append(line)

    def rect(self, x: float, y: float, w: float, h: float, cls: str, rx: float = 4) -> None:
        self.add(f'<rect class="{cls}" x="{x:g}" y="{y:g}" width="{w:g}" height="{h:g}" rx="{rx:g}"/>')

    def text(self, x: float, y: float, text: str, cls: str = '', anchor: str = 'start') -> None:
        c = f' class="{cls}"' if cls else ''
        a = f' text-anchor="{anchor}"' if anchor != 'start' else ''
        self.add(f'<text x="{x:g}" y="{y:g}"{c}{a}>{esc(text)}</text>')

    def path(self, d: str, cls: str, marker: bool = False) -> None:
        m = f' marker-end="url(#{self.uid}-arrow)"' if marker else ''
        self.add(f'<path class="{cls}" d="{d}"{m}/>')

    def arrow_marker(self) -> None:
        self.add(f'<defs><marker id="{self.uid}-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" '
                 f'markerHeight="7" orient="auto-start-reverse"><path class="d-arrowhead" d="M0,0 L10,5 L0,10 z"/>'
                 f'</marker></defs>')

    def write(self, name: str, out: Path = OUT) -> None:
        out.mkdir(parents=True, exist_ok=True)
        (out / f'{name}.svg').write_text('\n'.join(self.lines + ['</svg>']) + '\n')


def anatomy() -> None:
    """The anatomy of a VTK XML file: the data inside each element (inline) or in the appended section."""
    s = Svg(760, 450, 'The structure of a VTK XML file, with its data inline or appended', 'anat')
    s.arrow_marker()
    sections = [('<PointData>', 'DataArray Name="pressure"'), ('<CellData>', 'DataArray Name="id"'),
                ('<Points>', 'DataArray Name="Points"'), ('<Cells>', 'DataArray x 3')]
    segments = [64, 34, 80, 96]  # widths of the data of each section in the appended section
    for p, (x0, title, appended) in enumerate([(10, 'Inline: ascii, binary', False),
                                                 (390, 'Appended: raw, binary-appended', True)]):
        s.text(x0 + 180, 20, title, 'd-title', 'middle')
        s.rect(x0, 32, 360, 370, 'd-frame', 6)
        s.text(x0 + 12, 50, '<VTKFile type="UnstructuredGrid" ...>', 'd-mono')
        s.rect(x0 + 12, 60, 336, 278, 'd-box')
        s.text(x0 + 24, 78, '<UnstructuredGrid>', 'd-mono')
        s.rect(x0 + 24, 86, 312, 244, 'd-frame')
        s.text(x0 + 36, 104, '<Piece NumberOfPoints NumberOfCells>', 'd-mono')
        for i, (tag, array) in enumerate(sections):
            y = 114 + i * 54
            s.rect(x0 + 36, y, 288, 46, 'd-box', 3)
            s.text(x0 + 46, y + 15, tag, 'd-mono d-muted')
            s.rect(x0 + 48, y + 21, 264, 20, 'd-cell', 3)
            s.text(x0 + 56, y + 35, array, 'd-mono')
            if not appended:
                s.rect(x0 + 238, y + 25, 68, 12, 'd-data', 2)
            else:
                s.text(x0 + 306, y + 35, 'offset', 'd-mono d-neon-text', 'end')
        if not appended:
            s.text(x0 + 180, 362, 'each DataArray holds its own data:', '', 'middle')
            s.text(x0 + 180, 380, 'text (ascii) or base64 (binary)', 'd-muted', 'middle')
        else:
            s.rect(x0 + 12, 346, 336, 46, 'd-box')
            s.text(x0 + 22, 384, '<AppendedData encoding="raw"> _', 'd-mono d-muted')
            x = x0 + 24
            for i, w in enumerate(segments):
                s.rect(x, 356, 8, 14, 'd-head', 1)
                s.rect(x + 8, 356, w - 8, 14, 'd-data', 1)
                y = 114 + i * 54 + 31
                s.path(f'M{x0 + 312:g},{y:g} C{x0 + 352 - i * 4:g},{y:g} {x + w / 2:g},{300:g} {x + w / 2:g},{354:g}', 'd-link',
                       marker=True)
                x += w + 6
    s.text(380, 422, 'appended: each array is a bytes count header (UInt32 or UInt64) followed by its bytes;', 'd-muted',
           'middle')
    s.text(380, 440, 'the offset of each DataArray is where its header starts, after the "_"', 'd-muted', 'middle')
    s.write('anatomy')


def connectivity() -> None:
    """An unstructured grid: points, connectivity, offsets and cell types of a hexahedron and a pyramid."""
    s = Svg(760, 330, 'The connectivity, offsets and cell types of a hexahedron and a pyramid', 'conn')
    s.arrow_marker()
    # oblique projection of the points of ref_topologies.f90: a unit cube, the apex of a pyramid on its top face
    pts = [(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0), (0, 0, 1), (1, 0, 1), (1, 1, 1), (0, 1, 1), (0.5, 0.5, 1.8)]

    def proj(p: tuple[float, float, float]) -> tuple[float, float]:
        x, y, z = p
        return 60 + 130 * x + 56 * y, 280 - 112 * z - 42 * y

    xy = [proj(p) for p in pts]
    hidden = [(0, 3), (3, 2), (3, 7)]
    edges = [(0, 1), (1, 2), (4, 5), (5, 6), (6, 7), (7, 4), (0, 4), (1, 5), (2, 6), (4, 8), (5, 8), (6, 8), (7, 8)]
    for a, b in hidden:
        s.path(f'M{xy[a][0]:.1f},{xy[a][1]:.1f} L{xy[b][0]:.1f},{xy[b][1]:.1f}', 'd-edge-hidden')
    for a, b in edges:
        s.path(f'M{xy[a][0]:.1f},{xy[a][1]:.1f} L{xy[b][0]:.1f},{xy[b][1]:.1f}', 'd-edge')
    offsets = {0: (-14, 14), 1: (6, 16), 2: (10, 4), 3: (-14, -4), 4: (-16, 4), 5: (8, 16), 6: (10, -2), 7: (-16, -6),
               8: (8, -6)}
    for i, (x, y) in enumerate(xy):
        s.add(f'<circle class="d-point" cx="{x:.1f}" cy="{y:.1f}" r="3"/>')
        s.text(x + offsets[i][0], y + offsets[i][1], str(i), 'd-mono')
    s.text(150, 318, 'hexahedron (12) and pyramid (14)', 'd-muted', 'middle')
    # the arrays: the hexahedron in the seal colour, the pyramid in neon
    x0, cw = 400, 26
    rows = [('connectivity', [0, 1, 2, 3, 4, 5, 6, 7, 4, 5, 6, 7, 8], 40, 8), ('offset', [8, 13], 170, 1),
            ('cell_type', [12, 14], 236, 1)]
    for name, values, y, first in rows:
        s.text(x0 - 10, y + 18, name, 'd-mono d-muted', 'end')
        for j, v in enumerate(values):
            s.rect(x0 + j * cw, y, cw - 3, 26, 'd-seal-soft' if j < first else 'd-neon-soft', 3)
            s.text(x0 + j * cw + (cw - 3) / 2, y + 18, str(v), 'd-mono', 'middle')
    s.text(x0 - 10, 96, 'cell ends', 'd-mono d-muted', 'end')
    for end in (8, 13):  # the end of each cell: after its last id in connectivity
        xb = x0 + end * cw - 1.5
        s.path(f'M{xb:.1f},{36:g} L{xb:.1f},{72:g}', 'd-link')
        s.text(xb, 96, str(end), 'd-mono d-neon-text', 'middle')
    for j, end in enumerate((8, 13)):  # an offset points to the end of its cell
        xa, xb = x0 + j * cw + (cw - 3) / 2, x0 + end * cw - 1.5
        s.path(f'M{xa:.1f},{168:g} C{xa:.1f},{130:g} {xb:.1f},{140:g} {xb:.1f},{104:g}', 'd-link', marker=True)
    s.text(x0, 292, 'cell i: connectivity(offset(i-1)+1 : offset(i))', 'd-mono')
    s.text(x0, 312, 'point ids from 0, in the order of the cell type', 'd-muted')
    s.write('connectivity')


def pieces() -> None:
    """Pieces: the extents of structured pieces sharing a plane, unstructured pieces with ghost cells."""
    s = Svg(760, 330, 'The extents of two structured pieces, and two unstructured pieces with their ghost cells', 'pcs')
    s.add('<defs><pattern id="d-hatch" width="6" height="6" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">'
          '<line class="d-hatch-line" x1="0" y1="0" x2="0" y2="6"/></pattern></defs>')
    # structured: the whole extent 0..9 x 0..5, piece 1 i=0..4, piece 2 i=4..9: they share the points of the plane i=4
    s.text(190, 20, 'Structured pieces: extents', 'd-title', 'middle')
    x0, y0, c = 20, 60, 34
    for i in range(9):
        for j in range(5):
            s.rect(x0 + i * c, y0 + (4 - j) * c, c, c, 'd-seal-soft' if i < 4 else 'd-neon-soft', 0)
    s.path(f'M{x0 + 4 * c:g},{y0 - 6:g} L{x0 + 4 * c:g},{y0 + 5 * c + 6:g}', 'd-shared')
    for i in (0, 4, 9):
        s.text(x0 + i * c, y0 + 5 * c + 22, f'i={i}', 'd-mono', 'middle')
    s.text(x0 + 2 * c, y0 - 12, 'piece 1: i=0..4', 'd-mono', 'middle')
    s.text(x0 + 6.5 * c, y0 - 12, 'piece 2: i=4..9', 'd-mono', 'middle')
    s.text(x0, y0 + 5 * c + 44, 'whole extent i=0..9: the points of i=4', 'd-muted')
    s.text(x0, y0 + 5 * c + 62, 'belong to both pieces (nx2 of one = nx1 of the next)', 'd-muted')
    # unstructured: 8 cells in a row; each piece holds its own 4 cells and one ghost cell, a copy of its neighbour's
    s.text(575, 20, 'Unstructured pieces: ghost cells', 'd-title', 'middle')
    x1, w = 400, 38
    for p, (y, owned, ghost, cls) in enumerate([(70, range(0, 4), 4, 'd-seal-soft'), (190, range(4, 8), 3, 'd-neon-soft')]):
        s.text(x1, y - 10, f'piece {p + 1}', 'd-mono')
        cells = sorted(list(owned) + [ghost])
        for k, cell in enumerate(cells):
            x = x1 + cell * w
            s.rect(x, y, w - 4, 40, 'd-ghost' if cell == ghost else cls, 2)
            s.text(x + (w - 4) / 2, y + 25, str(1 if cell == ghost else 0), 'd-mono', 'middle')
    s.text(x1, 268, 'vtkGhostType (UInt8): 0 own cell, 1 ghost cell', 'd-mono')
    s.text(x1, 288, 'header: ghost_level=1; readers do not draw ghosts,', 'd-muted')
    s.text(x1, 306, 'so the cells shared by two pieces are drawn once', 'd-muted')
    s.write('pieces')


def sizes() -> None:
    """The size of the file of chapter 2 of the tutorial in each format, from the output of its run."""
    rows = []
    for line in (EXAMPLES / 'output' / 'heat_2.ansi').read_text().splitlines():
        words = line.split()
        if len(words) == 4 and words[3].isdigit():
            rows.append((words[1], words[2], int(words[3])))
    largest = max(r[2] for r in rows)
    s = Svg(760, 48 + 40 * len(rows), 'The size of the same temperature field written in each format', 'size')
    x0, w = 250, 400
    s.text(x0, 20, 'file size, the same 24 x 24 x 24 temperature field', 'd-muted')
    for i, (fmt, compressor, size) in enumerate(rows):
        y = 34 + 40 * i
        label = fmt if compressor == 'none' else f'{fmt} + {compressor}'
        s.text(x0 - 12, y + 19, label, 'd-mono', 'end')
        s.rect(x0, y + 4, w * size / largest, 22, 'd-data' if compressor == 'none' else 'd-head', 3)
        s.text(x0 + w * size / largest + 8, y + 20, f'{size / 1024:.0f} KiB', 'd-mono')
    s.write('heat_2-sizes', EXAMPLES / 'images')


if __name__ == '__main__':
    import sys

    if '--charts' in sys.argv[1:]:
        sizes()
        print('docs_diagrams: charts written')
        sys.exit(0)
    anatomy()
    connectivity()
    pieces()
    print('docs_diagrams:', ', '.join(sorted(p.name for p in OUT.glob('*.svg'))))
