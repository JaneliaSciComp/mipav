package gov.nih.mipav.view.vtk.Examples.VolumeRendering.Java;
import gov.nih.mipav.view.vtk.vtk.util.VtkPanelContainer;
import gov.nih.mipav.view.vtk.vtk.util.VtkPanelUtil;
import gov.nih.mipav.view.vtk.vtk.util.VtkUtil;
import vtk.*;

import javax.swing.*;
import java.awt.*;

/**
 * This is a simple volume rendering example that uses a
 * vtkVolumeRayCast mapper.
 */
public class SimpleRayCast extends JComponent implements VtkPanelContainer {

  private vtkPanel renWin;

  public SimpleRayCast() {
    // Setup VTK rendering panel
    renWin = new vtkPanel();

    // Create the reader for the data
    vtkStructuredPointsReader reader = new vtkStructuredPointsReader();
    reader.SetFileName(VtkUtil.getVtkDataRoot() + "/Data/ironProt.vtk");

    // Create transfer mapping scalar value to opacity
    vtkPiecewiseFunction opacityTransferFunction = new vtkPiecewiseFunction();
    opacityTransferFunction.AddPoint(20, 0.0);
    opacityTransferFunction.AddPoint(255, 0.2);

    // Create transfer mapping scalar value to color
    vtkColorTransferFunction colorTransferFunction = new vtkColorTransferFunction();
    colorTransferFunction.AddRGBPoint(0.0, 0.0, 0.0, 0.0);
    colorTransferFunction.AddRGBPoint(64.0, 1.0, 0.0, 0.0);
    colorTransferFunction.AddRGBPoint(128.0, 0.0, 0.0, 1.0);
    colorTransferFunction.AddRGBPoint(192.0, 0.0, 1.0, 0.0);
    colorTransferFunction.AddRGBPoint(255.0, 0.0, 0.2, 0.0);

    // The property describes how the data will look
    vtkVolumeProperty volumeProperty = new vtkVolumeProperty();
    volumeProperty.SetColor(colorTransferFunction);
    volumeProperty.SetScalarOpacity(opacityTransferFunction);
    volumeProperty.ShadeOn();
    volumeProperty.SetInterpolationTypeToLinear();

    // The mapper / ray cast function know how to render the data
    vtkVolumeRayCastCompositeFunction compositeFunction = new vtkVolumeRayCastCompositeFunction();
    vtkVolumeRayCastMapper volumeMapper = new vtkVolumeRayCastMapper();
    volumeMapper.SetVolumeRayCastFunction(compositeFunction);
    volumeMapper.SetInput(reader.GetOutput());

    // The volume holds the mapper and the property and
    // can be used to position/orient the volume
    vtkVolume volume = new vtkVolume();
    volume.SetMapper(volumeMapper);
    volume.SetProperty(volumeProperty);

    renWin.GetRenderer().AddVolume(volume);
    renWin.GetRenderer().SetBackground(1, 1, 1);
    VtkPanelUtil.setSize(renWin, 600, 600);

//    renWin.GetRenderWindow().AddObserver("AbortCheckEvent", this, "checkAbort");

    setLayout(new BorderLayout());
    add(renWin, BorderLayout.CENTER);
  }

  /**
   *
   */
  public void checkAbort() {
    if (renWin.GetRenderWindow().GetEventPending() != 0)
      renWin.GetRenderWindow().SetAbortRender(1);
  }

  public vtkPanel getRenWin() {
    return renWin;
  }

  public static void main(String s[]) {
    SimpleRayCast panel = new SimpleRayCast();

    JFrame frame = new JFrame("SimpleRayCast");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.getContentPane().add("Center", panel);
    frame.pack();
    frame.setVisible(true);
  }
}

