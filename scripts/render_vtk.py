"""Render a VTK file written by a documentation example: a PNG image, or an animated GIF for a .pvd time series.

Usage: pvpython --force-offscreen-rendering render_vtk.py FILE IMAGE [KEY=VALUE ...]

IMAGE is the image path without extension: IMAGE.png is written, or IMAGE.gif when FILE is a .pvd file with more than
one time step. The keys (all optional):

  array=NAME       colour by the point or cell array NAME (default: the solid colour of the site)
  edges=1          draw the cell edges
  outline=1        draw the bounding box of the dataset
  clip=X|Y|Z       cut away the half of the dataset beyond the middle plane normal to that axis, showing its inside
  contour=N        draw N isosurfaces of the point array NAME (instead of the surface)
  glyph=NAME       draw arrows of the point vectors NAME on a subset of the points
  camera=iso|xy|xz|yz   the view direction (default iso); zoom=F zooms in by F
  range=MIN,MAX    the colour range (default: the range of the array, over all the time steps)
  cmap=NAME        a ParaView colour map preset (default Inferno)
  categorical=1    colour an integer array by value, one colour each (e.g. the piece of each cell)
  title=TEXT       the title of the colour bar (default: the array name; underscores read as blanks)
  size=WxH         the image size in pixels (default 800x480); fps=N the frames per second of a GIF (default 8)

The renders are deterministic for a given ParaView version: the camera, colours and sizes depend only on the keys and on
the data. Used by scripts/docs_examples.sh for the "!render" markers: the images are generated, never edited by hand.
"""

from __future__ import annotations

import os
import sys

from paraview import simple as pv

BACKGROUND = [0.106, 0.106, 0.122]  # the dark background of the site (#1b1b1f)
SOLID = [0.392, 0.427, 0.937]  # the brand colour of the site, for uncoloured surfaces
EDGES = [0.85, 0.85, 0.9]
TEXT = [0.9, 0.9, 0.9]


def options(words: list[str]) -> dict[str, str]:
    """Parse the KEY=VALUE words."""
    parsed = {}
    for word in words:
        key, _, value = word.partition('=')
        parsed[key] = value
    return parsed


def association(source, name: str) -> str:
    """Return POINTS or CELLS: where the array NAME is."""
    info = source.GetDataInformation()
    if info.GetPointDataInformation().GetArrayInformation(name) is not None:
        return 'POINTS'
    if info.GetCellDataInformation().GetArrayInformation(name) is not None:
        return 'CELLS'
    raise SystemExit(f'render_vtk: no array {name}')


def data_range(source, name: str, assoc: str, times: list[float]) -> tuple[float, float]:
    """Return the range of the array (its magnitude, for vectors) over all the time steps."""
    lo, hi = float('inf'), float('-inf')
    for time in times or [None]:
        if time is not None:
            source.UpdatePipeline(time)
        info = source.GetDataInformation()
        data = info.GetPointDataInformation() if assoc == 'POINTS' else info.GetCellDataInformation()
        array = data.GetArrayInformation(name)
        component = -1 if array.GetNumberOfComponents() > 1 else 0
        r = array.GetComponentRange(component)
        lo, hi = min(lo, r[0]), max(hi, r[1])
    return lo, hi


def time_steps(reader) -> list[float]:
    """Return the time steps of the reader (none for a static file)."""
    times = getattr(reader, 'TimestepValues', None)
    if times is None:
        return []
    if isinstance(times, (int, float)):
        return [float(times)]
    return [float(t) for t in times]


def colour(display, view, source, name: str, assoc: str, times: list[float], opt: dict[str, str]) -> None:
    """Colour the display by the array, with its colour bar."""
    pv.ColorBy(display, (assoc, name))
    lut = pv.GetColorTransferFunction(name)
    lo, hi = data_range(source, name, assoc, times)
    if opt.get('categorical') == '1':
        lut.InterpretValuesAsCategories = 1
        lut.ApplyPreset('Brewer Qualitative Set3', True)
        values = [str(v) for v in range(int(lo), int(hi) + 1)]
        lut.Annotations = [s for v in values for s in (v, v)]
        lut.IndexedColors = lut.IndexedColors[:3 * len(values)]
    else:
        lut.ApplyPreset(opt.get('cmap', 'Inferno'), True)
        if 'range' in opt:
            lo, hi = (float(v) for v in opt['range'].split(','))
        lut.AutomaticRescaleRangeMode = 'Never'
        lut.RescaleTransferFunction(lo, hi)
    bar = pv.GetScalarBar(lut, view)
    bar.Title = opt.get('title', name).replace('_', ' ')
    bar.ComponentTitle = ''
    bar.TitleColor = TEXT
    bar.LabelColor = TEXT
    bar.TitleFontSize = 14
    bar.LabelFontSize = 12
    bar.ScalarBarLength = 0.5
    bar.WindowLocation = 'Any Location'
    bar.Position = [0.87, 0.25]
    display.SetScalarBarVisibility(view, True)


def main() -> None:
    path, image = sys.argv[1], sys.argv[2]
    opt = options(sys.argv[3:])
    width, height = (int(v) for v in opt.get('size', '800x480').split('x'))

    reader = pv.OpenDataFile(path)
    times = time_steps(reader)
    reader.UpdatePipeline(times[0] if times else 0.0)
    source = pv.MergeBlocks(Input=reader) if path.endswith('.vtm') else reader

    view = pv.CreateView('RenderView')
    view.ViewSize = [width, height]
    view.OrientationAxesVisibility = 0
    view.UseColorPaletteForBackground = 0
    view.Background = BACKGROUND

    name = opt.get('array', '')
    assoc = association(source, name) if name else ''
    shown = source
    bounds = source.GetDataInformation().GetBounds()
    if 'clip' in opt:
        normal = [0.0, 0.0, 0.0]
        normal['XYZ'.index(opt['clip'].upper())] = 1.0
        shown = pv.Clip(Input=source, ClipType='Plane', Crinkleclip=1)
        shown.ClipType.Origin = [(bounds[0] + bounds[1]) / 2, (bounds[2] + bounds[3]) / 2, (bounds[4] + bounds[5]) / 2]
        shown.ClipType.Normal = normal
    if 'contour' in opt:
        lo, hi = data_range(source, name, assoc, times)
        n = int(opt['contour'])
        shown = pv.Contour(Input=shown, ContourBy=['POINTS', name],
                           Isosurfaces=[lo + (hi - lo) * (i + 1) / (n + 1) for i in range(n)])

    display = pv.Show(shown, view)
    display.Representation = 'Surface With Edges' if opt.get('edges') == '1' else 'Surface'
    display.EdgeColor = EDGES
    if name:
        colour(display, view, source, name, assoc, times, opt)
    else:
        pv.ColorBy(display, None)
        display.AmbientColor = SOLID
        display.DiffuseColor = SOLID
    if opt.get('outline') == '1':
        outline = pv.Show(pv.Outline(Input=source), view)
        pv.ColorBy(outline, None)
        outline.DiffuseColor = EDGES
    if 'glyph' in opt:
        glyph = pv.Glyph(Input=source, GlyphType='Arrow', OrientationArray=['POINTS', opt['glyph']],
                         ScaleArray=['POINTS', 'No scale array'], GlyphMode='Every Nth Point')
        glyph.Stride = max(1, source.GetDataInformation().GetNumberOfPoints() // 400)
        glyph.ScaleFactor = 0.06 * max(bounds[1] - bounds[0], bounds[3] - bounds[2], bounds[5] - bounds[4])
        arrows = pv.Show(glyph, view)
        pv.ColorBy(arrows, None)
        arrows.DiffuseColor = TEXT
        arrows.AmbientColor = TEXT

    directions = {'xy': ([0, 0, 1], [0, 1, 0]), 'xz': ([0, -1, 0], [0, 0, 1]), 'yz': ([1, 0, 0], [0, 0, 1]),
                  'iso': ([1, -1.25, 0.9], [0, 0, 1])}
    position, up = directions[opt.get('camera', 'iso')]
    view.CameraFocalPoint = [0, 0, 0]
    view.CameraPosition = position
    view.CameraViewUp = up
    view.ResetCamera()
    view.GetActiveCamera().Zoom(float(opt.get('zoom', '1')))

    if len(times) > 1 and path.endswith('.pvd'):
        from PIL import Image  # bundled with pvpython

        frames = []
        frame_png = image + '.frame.png'
        for time in times:
            view.ViewTime = time
            pv.SaveScreenshot(frame_png, view, ImageResolution=[width, height])
            frames.append(Image.open(frame_png).convert('RGB').convert('P', palette=Image.Palette.ADAPTIVE, colors=128))
        os.remove(frame_png)
        frames[0].save(image + '.gif', save_all=True, append_images=frames[1:], loop=0, optimize=True,
                       duration=int(1000 / float(opt.get('fps', '8'))))
    else:
        pv.SaveScreenshot(image + '.png', view, ImageResolution=[width, height], CompressionLevel='9')


if __name__ == '__main__':
    main()
