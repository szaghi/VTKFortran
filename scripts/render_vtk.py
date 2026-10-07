"""Render a VTK file written by a documentation example: a PNG image, or an animated GIF for a .pvd time series.

Usage: pvpython --force-offscreen-rendering render_vtk.py FILE IMAGE [KEY=VALUE ...]

IMAGE is the image path without extension: IMAGE.png is written, or IMAGE.gif when FILE is a .pvd file with more than
one time step. The keys (all optional):

  array=NAME       colour by the point or cell array NAME (default: the solid colour of the site)
  edges=1          draw the cell edges
  opacity=F        the opacity of the surfaces, from 0 to 1 (default 1)
  points=N         draw the vertices (and points) as spheres of N pixels
  overlay=FILE     draw also FILE (next to the rendered one), not clipped, in a solid light colour: e.g. probes
  outline=1        draw the bounding box of the dataset
  clip=X|Y|Z[,F]   cut the dataset with the plane normal to that axis at the fraction F (default 0.5) of its extent,
                   keeping the part facing the camera: the cut shows the inside; crinkle=1 keeps whole cells
  contour=N        draw N isosurfaces of the point array NAME (instead of the surface)
  carpet=X|Y|Z[,F] draw the slice normal to that axis at the middle, lifted along the axis by the point array NAME:
                   a carpet plot, its height F (default 0.5) of the extent at the maximum of the array (of range)
  glyph=NAME       draw arrows of the point vectors NAME, scaled by their magnitude: on the cut plane with clip, on a
                   subset of the points otherwise
  camera=iso|xy|xz|yz   the view direction (default iso); zoom=F zooms in by F
  range=MIN,MAX    the colour range (default: the range of the array, over all the time steps)
  cmap=NAME        a ParaView colour map preset (default Turbo)
  categorical=1    colour an integer array by value, one colour each (e.g. the piece of each cell)
  log=1            a logarithmic colour scale (the range starts at 1e-3 of its maximum, at least)
  time=1           print the time of the dataset (the time step of a .pvd) in a corner
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
EDGES = [0.42, 0.42, 0.5]
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
        lut.ApplyPreset(opt.get('cmap', 'Turbo'), True)
        if 'range' in opt:
            lo, hi = (float(v) for v in opt['range'].split(','))
        lut.AutomaticRescaleRangeMode = 'Never'
        if opt.get('log') == '1':
            lo = max(lo, 1e-3 * hi)
            lut.RescaleTransferFunction(lo, hi)
            lut.UseLogScale = 1
        else:
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
    source.UpdatePipeline(times[0] if times else 0.0)

    view = pv.CreateView('RenderView')
    view.ViewSize = [width, height]
    view.OrientationAxesVisibility = 0
    view.UseColorPaletteForBackground = 0
    view.Background = BACKGROUND

    name = opt.get('array', '')
    assoc = association(source, name) if name else ''
    shown = source
    bounds = source.GetDataInformation().GetBounds()
    directions = {'xy': ([0, 0, 1], [0, 1, 0]), 'xz': ([0, -1, 0], [0, 0, 1]), 'yz': ([1, 0, 0], [0, 0, 1]),
                  'iso': ([1, -1.25, 0.9], [0, 0, 1])}
    position, up = directions[opt.get('camera', 'iso')]
    plane = None
    if 'clip' in opt:
        axis_name, _, fraction = opt['clip'].partition(',')
        axis = 'XYZ'.index(axis_name.upper())
        normal = [0.0, 0.0, 0.0]
        normal[axis] = 1.0
        origin = [(bounds[0] + bounds[1]) / 2, (bounds[2] + bounds[3]) / 2, (bounds[4] + bounds[5]) / 2]
        origin[axis] = bounds[2 * axis] + float(fraction or 0.5) * (bounds[2 * axis + 1] - bounds[2 * axis])
        # keep the part on the side of the camera: its cut face looks at the camera
        shown = pv.Clip(Input=source, ClipType='Plane', Invert=0 if position[axis] < 0 else 1,
                        Crinkleclip=1 if opt.get('crinkle') == '1' else 0)
        shown.ClipType.Origin = origin
        shown.ClipType.Normal = normal
        plane = (origin, normal)
    if 'carpet' in opt:
        axis_name, _, lift = opt['carpet'].partition(',')
        axis = 'XYZ'.index(axis_name.upper())
        normal = [0.0, 0.0, 0.0]
        normal[axis] = 1.0
        middle = [(bounds[0] + bounds[1]) / 2, (bounds[2] + bounds[3]) / 2, (bounds[4] + bounds[5]) / 2]
        cut = pv.Slice(Input=source, SliceType='Plane')
        cut.SliceType.Origin, cut.SliceType.Normal = middle, normal
        lo, hi = data_range(source, name, assoc, times)
        if 'range' in opt:  # the height follows the colour range: comparable between renders
            hi = float(opt['range'].split(',')[1])
        extent = max(bounds[1] - bounds[0], bounds[3] - bounds[2], bounds[5] - bounds[4])
        shown = pv.WarpByScalar(Input=cut, Scalars=['POINTS', name], Normal=normal, UseNormal=1,
                                ScaleFactor=float(lift or 0.5) * extent / (hi or 1.0))
    if 'contour' in opt:
        lo, hi = data_range(source, name, assoc, times)
        n = int(opt['contour'])
        shown = pv.Contour(Input=shown, ContourBy=['POINTS', name],
                           Isosurfaces=[lo + (hi - lo) * (i + 1) / (n + 1) for i in range(n)], ComputeScalars=1)

    display = pv.Show(shown, view)
    display.Representation = 'Surface With Edges' if opt.get('edges') == '1' else 'Surface'
    display.EdgeColor = EDGES
    display.Opacity = float(opt.get('opacity', '1'))
    if 'points' in opt:
        display.PointSize = float(opt['points'])
        display.RenderPointsAsSpheres = 1
    if name:
        colour(display, view, source, name, assoc, times, opt)
    else:
        display.ColorArrayName = ['POINTS', '']
        display.AmbientColor = SOLID
        display.DiffuseColor = SOLID
    if 'overlay' in opt:
        extra = pv.OpenDataFile(os.path.join(os.path.dirname(path), opt['overlay']))
        extra.UpdatePipeline()
        extra_display = pv.Show(extra, view)
        extra_display.PointSize = float(opt.get('points', '10'))
        extra_display.RenderPointsAsSpheres = 1
        # a solid colour: coloured as the surface, the overlay would not be seen on it
        extra_display.ColorArrayName = ['POINTS', '']
        extra_display.DiffuseColor = TEXT
        extra_display.AmbientColor = TEXT
    if opt.get('outline') == '1':
        outline = pv.Show(pv.Outline(Input=source), view)
        outline.ColorArrayName = ['POINTS', '']
        outline.DiffuseColor = EDGES
    if 'glyph' in opt:
        vectors = opt['glyph']
        seeds = source
        if plane is not None:
            # the arrows start just in front of the cut face, toward the camera, not hidden in it
            origin, normal = plane
            axis = normal.index(1.0)
            origin = list(origin)
            origin[axis] -= 0.03 * (bounds[2 * axis + 1] - bounds[2 * axis]) * (1 if position[axis] < 0 else -1)
            seeds = pv.Slice(Input=source, SliceType='Plane')
            seeds.SliceType.Origin, seeds.SliceType.Normal = origin, normal
        glyph = pv.Glyph(Input=seeds, GlyphType='Arrow', OrientationArray=['POINTS', vectors],
                         ScaleArray=['POINTS', vectors], GlyphMode='Every Nth Point')
        glyph.Stride = max(1, seeds.GetDataInformation().GetNumberOfPoints() // 300)
        largest = max(abs(v) for v in data_range(source, vectors, 'POINTS', times)) or 1.0
        glyph.ScaleFactor = 0.12 * max(bounds[1] - bounds[0], bounds[3] - bounds[2], bounds[5] - bounds[4]) / largest
        arrows = pv.Show(glyph, view)
        arrows.ColorArrayName = ['POINTS', '']
        arrows.DiffuseColor = TEXT
        arrows.AmbientColor = TEXT

    if opt.get('time') == '1':
        stamp = pv.Show(pv.AnnotateTimeFilter(Input=reader, Format='time = {time:.4f}'), view)
        stamp.FontSize = 16
        stamp.Color = TEXT
        stamp.WindowLocation = 'Upper Left Corner'
    view.CameraFocalPoint = [0, 0, 0]
    view.CameraPosition = position
    view.CameraViewUp = up
    view.ResetCamera()
    view.GetActiveCamera().Zoom(float(opt.get('zoom', '1')))

    if len(times) > 1 and path.endswith('.pvd'):
        from PIL import Image  # bundled with pvpython

        shots = []
        frame_png = image + '.frame.png'
        for time in times:
            view.ViewTime = time
            pv.SaveScreenshot(frame_png, view, ImageResolution=[width, height])
            shots.append(Image.open(frame_png).convert('RGB'))
        os.remove(frame_png)
        # one palette for all the frames, from the first and the last: no flicker, no stale regions
        palette = Image.new('RGB', (width, 2 * height))
        palette.paste(shots[0], (0, 0))
        palette.paste(shots[-1], (0, height))
        palette = palette.quantize(colors=255, dither=Image.Dither.NONE)
        frames = [shot.quantize(palette=palette, dither=Image.Dither.NONE) for shot in shots]
        frames[0].save(image + '.gif', save_all=True, append_images=frames[1:], loop=0, optimize=True,
                       duration=int(1000 / float(opt.get('fps', '8'))))
    else:
        pv.SaveScreenshot(image + '.png', view, ImageResolution=[width, height], CompressionLevel='9')


if __name__ == '__main__':
    main()
