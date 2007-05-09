package gov.nih.mipav.view.vtk.Examples.VolumeRendering.Java;
import gov.nih.mipav.view.vtk.vtk.util.VtkPanelContainer;
import gov.nih.mipav.view.vtk.vtk.util.VtkUtil;
import vtk.*;

import javax.swing.*;
import java.awt.*;

/**
 * This is a simple volume rendering example that uses a
 * vtkVolumeRayCast mapper.
 */
public class SimpleTextureMap2D extends JComponent implements VtkPanelContainer {

  private vtkPanel renWin;

  public SimpleTextureMap2D() {
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

    // The mapper knows how to render the data
    vtkVolumeTextureMapper2D volumeMapper = new vtkVolumeTextureMapper2D();
    volumeMapper.SetInput(reader.GetOutput());

    // The volume holds the mapper and the property and
    // can be used to position/orient the volume
    vtkVolume volume = new vtkVolume();
    volume.SetMapper(volumeMapper);
    volume.SetProperty(volumeProperty);

    renWin.GetRenderer().AddVolume(volume);

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
    SimpleTextureMap2D panel = new SimpleTextureMap2D();

    JFrame frame = new JFrame("SimpleTextureMap2D");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.getContentPane().add("Center", panel);
    frame.pack();
    frame.setVisible(true);
  }
}

