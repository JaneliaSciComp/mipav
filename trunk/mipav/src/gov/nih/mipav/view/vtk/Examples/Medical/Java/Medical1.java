package gov.nih.mipav.view.vtk.Examples.Medical.Java;
import gov.nih.mipav.view.vtk.vtk.util.VtkPanelContainer;
import gov.nih.mipav.view.vtk.vtk.util.VtkPanelUtil;
import gov.nih.mipav.view.vtk.vtk.util.VtkUtil;
import vtk.*;

import javax.swing.*;

import java.awt.*;

/**
 * This example reads a volume dataset, extracts an isosurface that
 * represents the skin and displays it.
 */
public class Medical1 extends JComponent implements VtkPanelContainer {

  private vtkPanel renWin;

  public Medical1() {
    // Create the buttons.
    renWin = new vtkPanel();


    // The following reader is used to read a series of 2D slices (images)
    // that compose the volume. The slice dimensions are set, and the
    // pixel spacing. The data Endianness must also be specified. The reader
    // usese the FilePrefix in combination with the slice number to construct
    // filenames using the format FilePrefix.%d. (In this case the FilePrefix
    // is the root name of the file: quarter.)
    vtkVolume16Reader v16 = new vtkVolume16Reader();
    v16.SetDataDimensions(64, 64);
    v16.SetDataByteOrderToLittleEndian();
    v16.SetFilePrefix(VtkUtil.getVtkDataRoot() + "/Data/headsq/quarter");
    v16.SetImageRange(1, 93);
    v16.SetDataSpacing(3.2, 3.2, 1.5);

    // An isosurface, or contour value of 500 is known to correspond to the
    // skin of the patient. Once generated, a vtkPolyDataNormals filter is
    // is used to create normals for smooth surface shading during rendering.
    // The triangle stripper is used to create triangle strips from the
    // isosurface these render much faster on some systems.
    vtkContourFilter skinExtractor = new vtkContourFilter();
    skinExtractor.SetInput(v16.GetOutput());
    skinExtractor.SetValue(0, 500);
    vtkPolyDataNormals skinNormals = new vtkPolyDataNormals();
    skinNormals.SetInput(skinExtractor.GetOutput());
    skinNormals.SetFeatureAngle(60.0);
    vtkPolyDataMapper skinMapper = new vtkPolyDataMapper();
    skinMapper.SetInput(skinNormals.GetOutput());
    skinMapper.ScalarVisibilityOff();
    vtkActor skin = new vtkActor();
    skin.SetMapper(skinMapper);

    // An outline provides context around the data.
    vtkOutlineFilter outlineData = new vtkOutlineFilter();
    outlineData.SetInput(v16.GetOutput());
    vtkPolyDataMapper mapOutline = new vtkPolyDataMapper();
    mapOutline.SetInput(outlineData.GetOutput());
    vtkActor outline = new vtkActor();
    outline.SetMapper(mapOutline);
    outline.GetProperty().SetColor(0, 0, 0);

    // It is convenient to create an initial view of the data. The FocalPoint
    // and Position form a vector direction. Later on (ResetCamera() method)
    // this vector is used to position the camera to look at the data in
    // this direction.
    vtkCamera aCamera = new vtkCamera();
    aCamera.SetViewUp(0, 0, -1);
    aCamera.SetPosition(0, 1, 0);
    aCamera.SetFocalPoint(0, 0, 0);
    aCamera.ComputeViewPlaneNormal();

    // Actors are added to the renderer. An initial camera view is created.
    // The Dolly() method moves the camera towards the FocalPoint,
    // thereby enlarging the image.
    renWin.GetRenderer().AddActor(outline);
    renWin.GetRenderer().AddActor(skin);
    renWin.GetRenderer().SetActiveCamera(aCamera);
    renWin.GetRenderer().ResetCamera();
    aCamera.Dolly(1.5);

    // Set a background color for the renderer and set the size of the
    // render window (expressed in pixels).
    renWin.GetRenderer().SetBackground(1, 1, 1);
    VtkPanelUtil.setSize(renWin, 640, 480);

    // Note that when camera movement occurs (as it does in the Dolly()
    // method), the clipping planes often need adjusting. Clipping planes
    // consist of two planes: near and far along the view direction. The
    // near plane clips out objects in front of the plane the far plane
    // clips out objects behind the plane. This way only what is drawn
    // between the planes is actually rendered.
    renWin.GetRenderer().ResetCameraClippingRange();

    // Setup panel
    setLayout(new BorderLayout());
    add(renWin, BorderLayout.CENTER);
  }


  public vtkPanel getRenWin() {
    return renWin;
  }

  public static void main(String s[]) {
    Medical1 panel = new Medical1();

    JFrame frame = new JFrame("Medical1");
    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    frame.getContentPane().add("Center", panel);
    frame.pack();
    frame.setVisible(true);
  }
}

