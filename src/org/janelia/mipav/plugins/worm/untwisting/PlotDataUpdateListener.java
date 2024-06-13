package org.janelia.mipav.plugins.worm.untwisting;

import java.util.List;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * Interface for handling updates to a plot panel. Classes that need to respond
 * to plot data changes should implement this interface. An instance of a class
 * implementing this interface can be registered with components that generate
 * plot data updates. When plot data changes occur, the {@code updatePlotPanel}
 * method of each registered listener is called, allowing the implementing class
 * to update the plot display accordingly.
 * 
 * @author diyi chen
 */
public interface PlotDataUpdateListener {

	/**
	 * Invoked to update the plot panel with new data.
	 * @param points 
	 * 
	 * @param values The list of data values to plot. Each value represents a data
	 *               point on the plot.
	 * @param title  The title of the plot, describing the data or context of what
	 *               is being displayed
	 */
	void updatePlotPanel(List<Vector3f> points, List<Float> values, String title);

}
