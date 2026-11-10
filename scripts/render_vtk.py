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
  labels=NAME      label each point with the value of its point array NAME
  celllabels=NAME[,DZ]  label each cell with the value of its cell array NAME, at its centre moved by DZ along z
  ghosts=show      draw the ghost cells too (marked by vtkGhostType, otherwise skipped), coloured by the array "ghost"
                   (0 own cell, 1 ghost cell) when array=ghost
  explode=F        for a parallel header (.pvtu): an animated GIF of its pieces moving apart from the centre (the farthest
                   one by the fraction F of the size of the dataset, the others in proportion), and back; their ghost
                   cells removed, so that each piece is drawn closed
  stride=N         for a .pvd GIF: one frame every N time steps (the last one always included)
  camera=front     a view from the front, a little from above and from the right (for galleries of cells)

The renders are deterministic for a given ParaView version: the camera, colours and sizes depend only on the keys and on
the data. Used by scripts/docs_examples.sh for the "!render" markers: the images are generated, never edited by hand.
"""

from __future__ import annotations

import math
import os
import sys

from paraview import simple as pv

# the night palette of the site (Circuit Moss, docs/.vitepress/theme/custom.css)
BACKGROUND = [0.027, 0.067, 0.059]  # ground, #07110f
SOLID = [0.098, 0.827, 1.0]  # neon, #19d3ff, for uncoloured surfaces
EDGES = [0.373, 0.478, 0.439]  # mesh, #5f7a70
TEXT = [0.902, 0.937, 0.914]  # ink, #e6efe9


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


def distinct_values(source, name: str, assoc: str) -> list[int]:
    """Return the distinct values of an integer array, sorted, over every partition of a partitioned dataset."""
    from paraview import servermanager
    from vtk.util.numpy_support import vtk_to_numpy
    import numpy

    data = servermanager.Fetch(source)
    if data.IsA('vtkPartitionedDataSet'):  # a VTKHDF file of several pieces
        parts = [data.GetPartition(i) for i in range(data.GetNumberOfPartitions())]
    else:
        parts = [data]
    values = []
    for part in parts:
        if part is None:
            continue
        arrays = part.GetPointData() if assoc == 'POINTS' else part.GetCellData()
        values.append(vtk_to_numpy(arrays.GetArray(name)))
    return [int(v) for v in numpy.unique(numpy.concatenate(values))]


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
        values = [str(v) for v in distinct_values(source, name, assoc)]
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
    bar.Position = [1 - 110 / view.ViewSize[0], 0.25]
    display.SetScalarBarVisibility(view, True)


def show_ghosts(source):
    """Return the dataset with its ghost cells drawn: vtkGhostType copied into "ghost", then dropped."""
    copy = pv.Calculator(Input=source, AttributeType='Cell Data', ResultArrayName='ghost', Function='vtkGhostType',
                         ResultArrayType='Int')
    names = [a.Name for a in copy.CellData if a.Name != 'vtkGhostType']
    kept = pv.PassArrays(Input=copy, CellDataArrays=names, PointDataArrays=[a.Name for a in copy.PointData])
    kept.UpdatePipeline()
    return kept


def labels(view, source, name: str, assoc: str, dz: float = 0.0) -> None:
    """Label the points (or the cell centres) with the values of an array."""
    from paraview import servermanager
    from vtkmodules.vtkFiltersCore import vtkCellCenters
    from vtkmodules.vtkRenderingCore import vtkActor2D
    from vtkmodules.vtkRenderingLabel import vtkLabeledDataMapper

    data = servermanager.Fetch(source)
    if assoc == 'CELLS':
        centres = vtkCellCenters()
        centres.SetInputData(data)
        centres.Update()
        data = centres.GetOutput()
        for i in range(data.GetNumberOfPoints()):
            x, y, z = data.GetPoint(i)
            data.GetPoints().SetPoint(i, x, y, z + dz)
    mapper = vtkLabeledDataMapper()
    mapper.SetInputData(data)
    mapper.SetLabelModeToLabelFieldData()
    mapper.SetFieldDataName(name)
    mapper.SetLabelFormat('%d')
    text = mapper.GetLabelTextProperty()
    text.SetColor(*(SOLID if assoc == 'CELLS' else TEXT))
    text.SetFontSize(20 if assoc == 'CELLS' else 15)
    text.SetBold(1 if assoc == 'CELLS' else 0)
    text.SetItalic(0)
    text.SetShadow(0)
    actor = vtkActor2D()
    actor.SetMapper(mapper)
    view.GetRenderer().AddActor2D(actor)


def explode(path: str, image: str, view, opt: dict[str, str], width: int, height: int) -> None:
    """Write a GIF of the pieces of a parallel header moving apart from the centre of the dataset, and back."""
    import xml.etree.ElementTree as ET
    from PIL import Image  # bundled with pvpython

    header = pv.OpenDataFile(path)
    header.UpdatePipeline()
    b = header.GetDataInformation().GetBounds()
    centre = [(b[0] + b[1]) / 2, (b[2] + b[3]) / 2, (b[4] + b[5]) / 2]
    size = max(b[1] - b[0], b[3] - b[2], b[5] - b[4])
    sources = [p.get('Source') for p in ET.parse(path).getroot().iter('Piece')]
    moved, directions = [], []
    for source in sources:
        piece = pv.RemoveGhostInformation(Input=pv.OpenDataFile(os.path.join(os.path.dirname(path), source)))
        piece.UpdatePipeline()
        p = piece.GetDataInformation().GetBounds()
        offset = [(p[0] + p[1]) / 2 - centre[0], (p[2] + p[3]) / 2 - centre[1], (p[4] + p[5]) / 2 - centre[2]]
        directions.append(offset)
        display = pv.Show(piece, view)
        display.Representation = 'Surface With Edges' if opt.get('edges') == '1' else 'Surface'
        display.EdgeColor = EDGES
        name = opt.get('array', '')
        if name:
            colour(display, view, header, name, association(header, name), [], opt)
        moved.append(display)
    farthest = max(max(sum(o * o for o in d) ** 0.5 for d in directions), 1e-30)
    directions = [[o / farthest for o in d] for d in directions]  # the farthest piece moves by explode*size
    position, up = {'xy': ([0, 0, 1], [0, 1, 0]), 'iso': ([1, -1.25, 0.9], [0, 0, 1])}[opt.get('camera', 'iso')]
    view.CameraFocalPoint = [0, 0, 0]
    view.CameraPosition = position
    view.CameraViewUp = up
    for m, d in zip(moved, directions):  # the camera frames the pieces at their farthest: the displays are moved,
        m.Translation = [v * float(opt['explode']) * size for v in d]  # not the data (no pipeline to update)
    view.ResetCamera()
    view.GetActiveCamera().Zoom(float(opt.get('zoom', '1')))
    n = 8
    steps = [0.0] * 2 + [0.5 - 0.5 * math.cos(math.pi * k / n) for k in range(1, n + 1)] + [1.0] * 2
    steps += steps[-2::-1]
    shots = []
    frame_png = image + '.frame.png'
    for f in steps:
        for m, d in zip(moved, directions):
            m.Translation = [v * f * float(opt['explode']) * size for v in d]
        view.StillRender()  # not pv.Render: its first call resets the camera
        pv.SaveScreenshot(frame_png, view, ImageResolution=[width, height])
        shots.append(Image.open(frame_png).convert('RGB'))
    os.remove(frame_png)
    palette = Image.new('RGB', (width, 2 * height))
    palette.paste(shots[0], (0, 0))
    palette.paste(shots[len(shots) // 2], (0, height))
    palette = palette.quantize(colors=255, dither=Image.Dither.NONE)
    frames = [shot.quantize(palette=palette, dither=Image.Dither.NONE) for shot in shots]
    frames[0].save(image + '.gif', save_all=True, append_images=frames[1:], loop=0, optimize=True,
                   duration=int(1000 / float(opt.get('fps', '8'))))


def main() -> None:
    path, image = sys.argv[1], sys.argv[2]
    opt = options(sys.argv[3:])
    width, height = (int(v) for v in opt.get('size', '800x480').split('x'))

    if 'explode' in opt:
        view = pv.CreateView('RenderView')
        view.ViewSize = [width, height]
        view.OrientationAxesVisibility = 0
        view.UseColorPaletteForBackground = 0
        view.Background = BACKGROUND
        explode(path, image, view, opt, width, height)
        return
    reader = pv.OpenDataFile(path)
    times = time_steps(reader)
    reader.UpdatePipeline(times[0] if times else 0.0)
    source = pv.MergeBlocks(Input=reader) if path.endswith('.vtm') else reader
    source.UpdatePipeline(times[0] if times else 0.0)
    if opt.get('ghosts') == 'show':
        source = show_ghosts(source)

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
                  'iso': ([1, -1.25, 0.9], [0, 0, 1]), 'front': ([0.3, -1, 0.35], [0, 0, 1])}
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

    if 'labels' in opt:
        labels(view, source, opt['labels'], 'POINTS')
    if 'celllabels' in opt:
        name_, _, dz = opt['celllabels'].partition(',')
        labels(view, source, name_, 'CELLS', float(dz or 0))
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
        stride = int(opt.get('stride', '1'))
        for time in times[::stride] + ([times[-1]] if (len(times) - 1) % stride else []):
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
