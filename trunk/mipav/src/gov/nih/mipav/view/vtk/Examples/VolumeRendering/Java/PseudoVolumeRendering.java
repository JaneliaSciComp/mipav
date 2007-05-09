package gov.nih.mipav.view.vtk.Examples.VolumeRendering.Java;
import gov.nih.mipav.view.vtk.vtk.util.VtkColors;
import gov.nih.mipav.view.vtk.vtk.util.VtkPanelContainer;
import gov.nih.mipav.view.vtk.vtk.util.VtkPanelUtil;
import gov.nih.mipav.view.vtk.vtk.util.VtkUtil;
import vtk.*;

import javax.swing.*;
import java.awt.*;

/**
 * Perform psuedo volume rendering in a structured grid by compositing
 * translucent cut planes. This same trick can be used for unstructured
 * grids. Note that for better results, more planes can be created. Also,
 * if your data is vtkImageData, there are much faster methods for volume
 * rendering.
 */
public class PseudoVolumeRendering extends JComponent implements VtkPanelContainer {

  private double opacity;
  private vtkCutter cutter;
  private vtkCamera cam1;
  private vtkPlane plane;
  private vtkLookupTable clut;
  private vtkPanel renWin;

  public PseudoVolumeRendering() {
    // Setup VTK rendering panel
    renWin = new vtkPanel();

    // Create pipeline. Read structured grid data.
    vtkPLOT3DReader pl3d = new vtkPLOT3DReader();
    pl3d.SetXYZFileName(VtkUtil.getVtkDataRoot() + "/Data/combxyz.bin");
    pl3d.SetQFileName(VtkUtil.getVtkDataRoot() + "/Data/combq.bin");
    pl3d.SetScalarFunctionNumber(100);
    pl3d.SetVectorFunctionNumber(202);
    pl3d.Update();

    // A convenience, use this filter to limit data for experimentation.
    vtkExtractGrid extract = new vtkExtractGrid();
    extract.SetVOI(1, 55, -1000, 1000, -1000, 1000);
    extract.SetInput(pl3d.GetOutput());

    // The (implicit); plane is used to do the cutting
    plane = new vtkPlane();
    plane.SetOrigin(0, 4, 2);
    plane.SetNormal(0, 1, 0);

    // The cutter is set up to process each contour value over all cells
    // (SetSortByToSortByCell);. This results in an ordered output of polygons
    // which is key to the compositing.
    cutter = new vtkCutter();
    cutter.SetInput(extract.GetOutput());
    cutter.SetCutFunction(plane);
    cutter.GenerateCutScalarsOff();
    cutter.SetSortByToSortByCell();

    clut = new vtkLookupTable();
    clut.SetHueRange(0, .67);
    clut.Build();

    vtkPolyDataMapper cutterMapper = new vtkPolyDataMapper();
    cutterMapper.SetInput(cutter.GetOutput());
    cutterMapper.SetScalarRange(.18, .7);
    cutterMapper.SetLookupTable(clut);

    vtkActor cut = new vtkActor();
    cut.SetMapper(cutterMapper);

    // Add in some surface geometry for interest.
    vtkContourFilter iso = new vtkContourFilter();
    iso.SetInput(pl3d.GetOutput());
    iso.SetValue(0, .22);
    vtkPolyDataNormals normals = new vtkPolyDataNormals();
    normals.SetInput(iso.GetOutput());
    normals.SetFeatureAngle(45);
    vtkPolyDataMapper isoMapper = new vtkPolyDataMapper();
    isoMapper.SetInput(normals.GetOutput());
    isoMapper.ScalarVisibilityOff();
    vtkActor isoActor = new vtkActor();
    isoActor.SetMapper(isoMapper);
    isoActor.GetProperty().SetDiffuseColor(VtkColors.TOMATO);
    isoActor.GetProperty().SetSpecularColor(VtkColors.WHITE);
    isoActor.GetProperty().SetDiffuse(.8);
    isoActor.GetProperty().SetSpecular(.5);
    isoActor.GetProperty().SetSpecularPower(30);

    vtkStructuredGridOutlineFilter outline = new vtkStructuredGridOutlineFilter();
    outline.SetInput(pl3d.GetOutput());
    vtkTubeFilter outlineTubes = new vtkTubeFilter();
    outlineTubes.SetInput(outline.GetOutput());
    outlineTubes.SetRadius(.1);

    vtkPolyDataMapper outlineMapper = new vtkPolyDataMapper();
    outlineMapper.SetInput(outlineTubes.GetOutput());
    vtkActor outlineActor = new vtkActor();
    outlineActor.SetMapper(outlineMapper);

    // Add the actors to the renderer, set the background and size
    renWin.GetRenderer().AddActor(outlineActor);
    outlineActor.GetProperty().SetColor(VtkColors.BANNANA);
    renWin.GetRenderer().AddActor(isoActor);
    isoActor.VisibilityOn();
    renWin.GetRenderer().AddActor(cut);
    opacity = .1;
    cut.GetProperty().SetOpacity(1);
    renWin.GetRenderer().SetBackground(1, 1, 1);
    VtkPanelUtil.setSize(renWin, 640, 480);

    cam1 = renWin.GetRenderer().GetActiveCamera();
    cam1.SetClippingRange(3.95297, 50);
    cam1.SetFocalPoint(9.71821, 0.458166, 29.3999);
    cam1.SetPosition(2.7439, -37.3196, 38.7167);
    cam1.ComputeViewPlaneNormal();
    cam1.SetViewUp(-0.16123, 0.264271, 0.950876);

    // Generate 20 cut planes
    generateCutPlanes(20);

    // Place VTK panel
    setLayout(new BorderLayout());
    add(renWin, BorderLayout.CENTER);
  }

  /**
   * Generates cut planes normal to camera's view plane
   * 
   * @param numberOfPlanes number of cut planes to generate.
   */
  private void generateCutPlanes(int numberOfPlanes) {
    plane.SetNormal(cam1.GetViewPlaneNormal());
    plane.SetOrigin(cam1.GetFocalPoint());
    cutter.GenerateValues(numberOfPlanes, -5, 5);
    clut.SetAlphaRange(opacity, opacity);
  }


  public vtkPanel getRenWin() {
    return renWin;
  }

  public static void main(String s[]) {
    PseudoVolumeRendering panel = new PseudoVolumeRendering();

    JFrame frame = new JFrame("PseudoVolumeRendering");
    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    frame.getContentPane().add("Center", panel);
    frame.pack();
    frame.setVisible(true);
  }
}

