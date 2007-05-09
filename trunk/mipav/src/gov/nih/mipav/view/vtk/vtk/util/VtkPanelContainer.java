package gov.nih.mipav.view.vtk.vtk.util;

import vtk.vtkPanel;

/**
 * Common interface for components containing a vtkPanel.
 * 
 * @author Jarek Sacha
 * @version $Revision: 1.4 $
 */
public interface VtkPanelContainer {

  /**
   * Get reference to vtkPanel hosted by this component.
   * 
   * @return Reference to vtkPanel hosted by this component.
   */
  public vtkPanel getRenWin();
}
