package gov.nih.mipav.view.vtk.vtk.util;

import vtk.*;

import javax.swing.*;
import java.awt.*;
import java.util.Observable;
import java.util.Observer;

/**
 * Wrapper for display of vtkImageViewer.
 */
public class ImageViewerPanel extends JComponent  implements VtkPanelContainer {

  private static class CustomVtkPanel extends vtkCanvas {
    private vtkImageViewer imageViewer = new vtkImageViewer();

    CustomVtkPanel() {
      super();
      Dimension dimension = super.getSize();
      rw = imageViewer.GetRenderWindow();
      rw.SetSize(dimension.width, dimension.height);
      ren = imageViewer.GetRenderer();
      imageViewer.GetRenderWindow().SetInteractor(iren);
    }
  }


  private CustomVtkPanel customVtkPanel;
  private vtkTextMapper textMapper;


  public ImageViewerPanel() {
    customVtkPanel = new CustomVtkPanel();

    textMapper = new vtkTextMapper();
    vtkTextActor textActor = new vtkTextActor();
    textActor.SetInput("?");
    textActor.SetMapper(textMapper);
    textActor.SetPosition(0.1, 0.9);

    customVtkPanel.GetRenderer().AddActor(textActor);

    setLayout(new BorderLayout());
    add(customVtkPanel, BorderLayout.CENTER);
  }

  public vtkTextMapper getTextMapper() {
    return textMapper;
  }


  public vtkActor2D getImageViewerActor2D() {
    return customVtkPanel.imageViewer.GetActor2D();
  }

  public void setImageViewerInput(vtkImageData imageData) {
    customVtkPanel.imageViewer.SetInput(imageData);
  }


  public int getImageViewerZSlice() {
    return customVtkPanel.imageViewer.GetZSlice();
  }


  public void setImageViewerZSlice(int zSlice) {
    customVtkPanel.imageViewer.SetZSlice(zSlice);
    customVtkPanel.repaint();
  }


  public void setImageViewerColorWindow(double colorWindow) {
    customVtkPanel.imageViewer.SetColorWindow(colorWindow);
  }


  public void setImageViewerColorLevel(double colorLevel) {
    customVtkPanel.imageViewer.SetColorLevel(colorLevel);
  }


  public vtkRenderWindowInteractor getRenderInteractor() {
    return customVtkPanel.imageViewer.GetRenderWindow().GetInteractor();
  }


  public vtkPanel getRenWin() {
    return customVtkPanel;
  }


  synchronized public void setSize(int width, int height) {
    super.setSize(width, height);
    // Verify that this method overwrite is needed once vtkPanel sizing problem is resolved
    if (width > 0 && height > 0) {
      customVtkPanel.setSize(width, height);
      if (!customVtkPanel.isWindowSet()) {
        customVtkPanel.addWindowSetObserver(new Observer() {
          public void update(Observable o, Object arg) {
            final Dimension d = customVtkPanel.getSize();
            customVtkPanel.setSize(d.width, d.height);
          }
        });
      }
    }
  }


  public void repaint() {
    super.repaint();
    if (customVtkPanel != null)
      customVtkPanel.repaint();
  }
}
