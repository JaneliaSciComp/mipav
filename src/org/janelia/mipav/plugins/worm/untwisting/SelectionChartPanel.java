package org.janelia.mipav.plugins.worm.untwisting;

import org.jfree.chart.*;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.event.MarkerChangeEvent;
import org.jfree.chart.event.MarkerChangeListener;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.ui.RectangleAnchor;
import org.jfree.chart.ui.TextAnchor;
import org.jfree.chart.ui.Layer;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewImageUpdateInterface;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class SelectionChartPanel extends ChartPanel implements MarkerChangeListener, ViewImageUpdateInterface {

/**
 * CustomChartPanel is a custom extension of the ChartPanel class from JFreeChart
 * used to display and interact with a dynamic XY line chart. It includes custom
 * behaviors for handling marker changes and mouse drag events.
 */
	private static final long serialVersionUID = 1L;
	private JFreeChart selectionChart;
	private List<Vector3f> chart3DPoints;
	private PlugInDialogVolumeRenderDualJanelia parent;
	private Color[] currentColors = { Color.YELLOW };
	private String[] channelNames;
	private List<Boolean> channelSelected = new ArrayList<>();

	/**
     * Constructor to initialize the chart panel with data.
     */
	public SelectionChartPanel(List<Float> values, String title, PlugInDialogVolumeRenderDualJanelia parent) {
		super(null);
		this.selectionChart = createChart(values, title, this);
		this.parent = parent;
		this.setChart(selectionChart);
		setPreferredSize(new Dimension(100,100));//  plot gets larger vertically and horizontally if remove this line
		initialize();

	}
	
	/**
     * Initializes chart panel settings.
     */
	private void initialize() {
		setMouseWheelEnabled(false); 
		setDomainZoomable(false);
		setRangeZoomable(false);
		setMouseZoomable(false, false);
		setFillZoomRectangle(false);
		setZoomAroundAnchor(false);
		
		// Add a mouse motion listener to handle dragging movements over the chart
		addMouseMotionListener(new MouseAdapter() {
			@Override
			public void mouseDragged(MouseEvent e) {
				// Get the plot area for accurate coordinate calculation
				Rectangle2D plotArea = getScreenDataArea();
				XYPlot plot = selectionChart.getXYPlot();
				ValueAxis xAxis = plot.getDomainAxis();
				double x = xAxis.java2DToValue(e.getX(), plotArea, plot.getDomainAxisEdge());
				ValueAxis yAxis = plot.getRangeAxis();
				double yClick = yAxis.java2DToValue(e.getY(), plotArea, plot.getRangeAxisEdge());
				
				// Determine the corresponding Y-value by finding the nearest index
				XYSeriesCollection dataset = (XYSeriesCollection) plot.getDataset();
				XYSeries series = dataset.getSeries(0);
				XYSeries slopeSeries = dataset.getSeries(1);
				
				if(series.getItemCount() == 0) return;
				int index = findNearestXIndex(series, x);
			    if(index >= 0 && index < series.getItemCount()) { // Ensure index is valid

				//int index = findNearestXIndex(series, x);
				double y = series.getY(index).doubleValue();
				double slope = slopeSeries.getY(index).doubleValue(); // slope is derivative at x

				// Update the marker's value and label on the chart
				ValueMarker marker = (ValueMarker) plot.getDomainMarkers(Layer.FOREGROUND).iterator().next();
				marker.setValue(x);
				marker.setLabel(String.format("Value: %.2f, Derivative: %.2f", y, slope));
				marker.setLabelFont(new Font("Serif", Font.BOLD, 14));
				marker.setStroke(new BasicStroke(2.0f)); 
				
				// Update the thresholdMarker's value and label on the chart
				ValueMarker thresholdMarker = (ValueMarker) plot.getRangeMarkers(Layer.FOREGROUND).iterator().next();
				thresholdMarker.setValue(yClick);
				thresholdMarker.setLabel(String.format("Threshold Value: %.2f", yClick));
				thresholdMarker.setLabelFont(new Font("Serif", Font.BOLD, 14));
				thresholdMarker.setStroke(new BasicStroke(2.0f)); 
				
				refreshPlot();

				// Redraw the chart to reflect changes
				repaint();
				e.consume();
			}
			}
		});
	}
	
	/**
     * Overrides the method to set a chart and configure its settings.
     */
	public void setChart(JFreeChart chart) {
	    super.setChart(chart);
	    setMouseWheelEnabled(false);
		setDomainZoomable(false);
		setRangeZoomable(false);
		setMouseZoomable(false, false);
		setFillZoomRectangle(false);
		setZoomAroundAnchor(false);
	}

	/**
	 * Finds the index of the closest X value to the given target x.
	 * 
	 * @param series The series of data points.
	 * @param x      The target x value to match.
	 * @return The index of the closest x value.
	 */
	private int findNearestXIndex(XYSeries series, double x) {
		double minDistance = Double.MAX_VALUE;
		int nearestIndex = -1;

		for (int i = 0; i < series.getItemCount(); i++) {
			double distance = Math.abs(series.getX(i).doubleValue() - x);
			if (distance < minDistance) {
				minDistance = distance;
				nearestIndex = i;
			}
		}
		return nearestIndex;
	}

	/**
     * Updates the chart with new data and title.
     */
	public void updateChart(List<Float> values, String title) {
		this.selectionChart = createChart(values, title, this);
		setChart(this.selectionChart);
		refreshPlot();
		revalidate();
		repaint();
	}
	
	/**
     * Updates the charts with new data and title.
     */
	public void updateCharts(List<List<Float>> values, List<Boolean> channelSelected) {
		this.channelSelected = channelSelected;
		this.selectionChart = createCharts(values, "Selection Chart", this);
		setChart(this.selectionChart);
		refreshPlot();
		revalidate();
		repaint();
	}


	// Set up a marker change listener to handle marker position changes
	@Override
	public void markerChanged(MarkerChangeEvent event) {
		ValueMarker marker = (ValueMarker) event.getMarker();
		float tq = (float) marker.getValue();
		// Calculate the indices for interpolation
		int t0 = (int) Math.floor(tq);
		int t1 = (int) Math.ceil(tq);

		if (t0 >= 0 && t1 < chart3DPoints.size() && t0 != t1) {
			Vector3f interpolatedPoint = interpolate(t0, t1, tq);
			parent.update3DModel(interpolatedPoint);
		} else if (t0 == t1 && t0 < chart3DPoints.size()) {
			Vector3f exactPoint = chart3DPoints.get(t0);
			parent.update3DModel(exactPoint);
		}
	}

	/**
	 * Interpolates between two 3D points based on a given interpolation parameter.
	 * 
	 * @param t0 Index of the first point.
	 * @param t1 Index of the second point.
	 * @param tq The interpolation parameter, typically derived from the marker's
	 *           position.
	 * @return The interpolated 3D point.
	 */
	private Vector3f interpolate(int t0, int t1, float tq) {
		Vector3f pt0 = chart3DPoints.get(t0);
		Vector3f pt1 = chart3DPoints.get(t1);

		float m0 = (t1 - tq) / (t1 - t0);
		float m1 = (tq - t0) / (t1 - t0);

		float x = (float) (m0 * pt0.X + m1 * pt1.X);
		float y = (float) (m0 * pt0.Y + m1 * pt1.Y);
		float z = (float) (m0 * pt0.Z + m1 * pt1.Z);

		return new Vector3f(x, y, z);
	}

	/**
	 * Creates a chart using a list of values and assigns it a title. Each value in
	 * the list is plotted against its index.
	 * 
	 * @param values List of floating-point values for the Y-axis.
	 * @param title  Title of the chart.
	 * @return A JFreeChart object fully initialized.
	 */
	private JFreeChart createChart(List<Float> values, String title, MarkerChangeListener markerListener) {
		XYSeries series = new XYSeries("Data");
		float maxValue = -Float.MAX_VALUE;
		int maxIndex = -1;

		// Populate the series with values and track the maximum value and its index
		for (int i = 0; i < values.size(); i++) {
			float value = values.get(i);
			series.add(i, value);
			if (value > maxValue) {
				maxValue = value;
				maxIndex = i;
			}
		}

		XYSeriesCollection dataset = new XYSeriesCollection();
		dataset.addSeries(series);
		
		// Calculate slopes and add to the series
		List<Float> slopes = calculateSecantSlopes(values);
		
	    XYSeries slopeSeries = new XYSeries("Slopes");
	    for (int i = 2; i < values.size(); i++) { 
	        double slopeIndex = i -1; // offset half an interval, 0.5, from the original values along the x-axis.
	        slopeSeries.add(slopeIndex, slopes.get(i-2));
	    }
	    dataset.addSeries(slopeSeries);

		// Create the chart
		JFreeChart chart = ChartFactory.createXYLineChart(title, "Index", "Value", dataset, PlotOrientation.VERTICAL,
				true, true, false);

		XYPlot plot = chart.getXYPlot();
		
		selectionChart = chart;
		setupRendererWithGradient(plot, 0);
		
		plot.setBackgroundPaint(Color.GRAY);
		plot.setDomainGridlinePaint(Color.DARK_GRAY);
		plot.setRangeGridlinePaint(Color.DARK_GRAY); 
		
		XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer();
		renderer.setSeriesPaint(0, Color.YELLOW);
		renderer.setSeriesStroke(0, new BasicStroke(2.0f));
		plot.setRenderer(renderer);

		// Added vertical domianMarker
		ValueMarker marker = new ValueMarker(maxIndex);
		marker.setPaint(Color.CYAN);
		marker.setLabel("Max Value: " + maxValue);
		marker.setLabelFont(new Font("Serif", Font.BOLD, 14)); 
		marker.setLabelPaint(Color.WHITE);
		marker.setLabelAnchor(RectangleAnchor.CENTER);
		marker.setLabelTextAnchor(TextAnchor.CENTER_LEFT);
		marker.setStroke(new BasicStroke(2.0f)); 
		plot.addDomainMarker(marker);
		
		// Added horizontal theesholdMarker
		float thresholdValue = maxValue / 2;
		ValueMarker thresholdMarker = new ValueMarker(thresholdValue);
		thresholdMarker.setPaint(Color.CYAN);
		thresholdMarker.setLabel("Threshold Value: " + thresholdValue);
		thresholdMarker.setLabelFont(new Font("Serif", Font.BOLD, 14)); 
		thresholdMarker.setLabelPaint(Color.WHITE);
		thresholdMarker.setLabelAnchor(RectangleAnchor.CENTER);
		thresholdMarker.setLabelTextAnchor(TextAnchor.CENTER_LEFT);
		thresholdMarker.setStroke(new BasicStroke(2.0f)); 
		plot.addRangeMarker(thresholdMarker);
		
		marker.addChangeListener(markerListener);

		return chart;
	}
	
	/**
	 * Creates a chart using a list of values and assigns it a title. Each value in
	 * the list is plotted against its index.
	 * 
	 * @param values List of floating-point values for the Y-axis.
	 * @param title  Title of the chart.
	 * @return A JFreeChart object fully initialized.
	 */
	private JFreeChart createCharts(List<List<Float>> listOfValues, String title, MarkerChangeListener markerListener) {
		float maxValue = -Float.MAX_VALUE;
		int maxIndex = -1;
		
		XYSeriesCollection dataset = new XYSeriesCollection();
		
		for(int j = 0; j < listOfValues.size(); j++) {
			
			List<Float> values = listOfValues.get(j);
		
			//XYSeries series = new XYSeries("Data " + channelNames[channelSelected.get(j)]);
			XYSeries series = new XYSeries("Data " + channelNames[j]);
			
			// Populate the series with values and track the maximum value and its index
			for (int i = 0; i < values.size(); i++) {
				float value = values.get(i);
				series.add(i, value);
				if (value > maxValue) {
					maxValue = value;
					maxIndex = i;
				}
			}
			
	
			dataset.addSeries(series);
			
			// Calculate slopes and add to the series
			List<Float> slopes = calculateSecantSlopes(values);
			
		   // XYSeries slopeSeries = new XYSeries("Slopes " + channelNames[channelSelected.get(j)]);
		    XYSeries slopeSeries = new XYSeries("Slopes " + channelNames[j]);
		    for (int i = 2; i < values.size(); i++) { 
		        double slopeIndex = i -1; // offset half an interval, 0.5, from the original values along the x-axis.
		        slopeSeries.add(slopeIndex, slopes.get(i-2));
		    }
		    dataset.addSeries(slopeSeries);
	    
		}

		// Create the chart
		JFreeChart chart = ChartFactory.createXYLineChart(title, "Index", "Value", dataset, PlotOrientation.VERTICAL,
				true, true, false);

		XYPlot plot = chart.getXYPlot();
		
		selectionChart = chart;
		IntStream.range(0, listOfValues.size())
			.forEach(i -> setupRendererWithGradient(plot, i));
		
		plot.setBackgroundPaint(Color.GRAY);
		plot.setDomainGridlinePaint(Color.DARK_GRAY);
		plot.setRangeGridlinePaint(Color.DARK_GRAY); 
		
		XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer();
		renderer.setSeriesPaint(0, Color.YELLOW);
		renderer.setSeriesStroke(0, new BasicStroke(2.0f));
		plot.setRenderer(renderer);

		// Added vertical domianMarker
		ValueMarker marker = new ValueMarker(maxIndex);
		marker.setPaint(Color.CYAN);
		marker.setLabel("Max Value: " + maxValue);
		marker.setLabelFont(new Font("Serif", Font.BOLD, 14)); 
		marker.setLabelPaint(Color.WHITE);
		marker.setLabelAnchor(RectangleAnchor.CENTER);
		marker.setLabelTextAnchor(TextAnchor.CENTER_LEFT);
		marker.setStroke(new BasicStroke(2.0f)); 
		plot.addDomainMarker(marker); 
		
		// Added horizontal theesholdMarker
		float thresholdValue = maxValue / 2;
		ValueMarker thresholdMarker = new ValueMarker(thresholdValue);
		thresholdMarker.setPaint(Color.CYAN);
		thresholdMarker.setLabel("Threshold Value: " + thresholdValue);
		thresholdMarker.setLabelFont(new Font("Serif", Font.BOLD, 14)); 
		thresholdMarker.setLabelPaint(Color.WHITE);
		thresholdMarker.setLabelAnchor(RectangleAnchor.CENTER);
		thresholdMarker.setLabelTextAnchor(TextAnchor.CENTER_LEFT);
		thresholdMarker.setStroke(new BasicStroke(2.0f)); 
		plot.addRangeMarker(thresholdMarker);
		
		marker.addChangeListener(markerListener);

		return chart;
	}
	
	/**
	 * Calculates the slopes between points in the list
	 *
	 * @param values List of values for which slopes are to be calculated.
	 * @return List of calculated slopes.
	 */
	private static List<Float> calculateSecantSlopes(List<Float> values) {
	    List<Float> slopes = new ArrayList<>();
	    for (int i = 2; i < values.size(); i++) {
	        float slope = (values.get(i) - values.get(i - 2)) / 1.0f; 
	        slopes.add(slope);
	    }
	    return slopes;
	}
	

	/**
	 * Sets the 3D points corresponding to the data points in the plot.
	 *
	 * @param points List of 3D points to be used in corresponding 3D model updates.
	 */
	public void setChart3DPoints(List<Vector3f> points) {
		this.chart3DPoints = points;
	}

	/**
	 * Method to find the next peak. It cycles through values starting from a given
	 * index to identify a peak where the slope changes sign.
	 * 
	 * @param index     Starting index for the search, ensuring it doesn't start
	 *                  before the beginning of the list.
	 * @param values    List of Y-values from the dataset.
	 * @param threshold Minimum Y-value to consider for a peak.
	 * @return The index of the next peak if found; otherwise, returns -1.
	 */
	public float nextPeak(int index, List<Float> values, double threshold) {
		// Calculate slopes between each pair of points using a secant method.
		List<Float> slopes = calculateSecantSlopes(values);
		// Ensure the starting index is not less than zero
		index = Math.max(index, 0);

		// Loop through slopes to find where the sign changes.
		for (int j = 0; j < slopes.size() - 1; j++) {
			int i = (j + index) % (slopes.size() - 1);
			float currentSlope = slopes.get(i);
			float nextSlope = slopes.get(i + 1);
			float y = values.get(i + 1);

			// Check if current point is a peak by comparing it against the threshold and
			// slope changes.
			if (y > threshold && currentSlope > 0 && nextSlope < 0) {
				// Calculate interpolated index where the slope would cross zero.
				float indexX = ((-currentSlope) / (nextSlope - currentSlope)) + i;
				System.out.println("here is the indexX:" + indexX);
				System.out.println("this is i:" + i);
				return indexX + 1;

			}
		}
		System.out.println("No peak found.");
		return Float.POSITIVE_INFINITY;
	}

	/***
	 * Handles action events, specifically looking to handle "Next Peak" actions.
	 * 
	 * @param e The ActionEvent object containing details about the event.
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand() == "Next Peak") {
			XYPlot plot = selectionChart.getXYPlot();
			// Retrieve data and convert it into a List<Float> for processing
			XYSeriesCollection dataset = (XYSeriesCollection) plot.getDataset();

			// Access current marker position and determine the next index to check for a
			// peak
			ValueMarker marker = (ValueMarker) plot.getDomainMarkers(Layer.FOREGROUND).iterator().next();
			int index = (int) Math.ceil(marker.getValue()) + 1;
			
			// Retrieve threshold value from the threshold marker
			ValueMarker thresholdMarker = (ValueMarker) plot.getRangeMarkers(Layer.FOREGROUND).iterator().next();
			double threshold = thresholdMarker.getValue();
			
			int nSeries = dataset.getSeriesCount();
			
			float peakIndex = Float.POSITIVE_INFINITY;
			float peakValue = 0;
			float peakDistance = Float.POSITIVE_INFINITY;
			
			for (int s=0; s < nSeries; s += 2) {
				if (channelSelected.get(s/2)) {
					XYSeries series = dataset.getSeries(s);
					double[][] data = series.toArray();
					List<Float> values = Arrays.stream(data[1]).mapToObj(d -> Float.valueOf((float) d))
						.collect(Collectors.toList());
					float seriesPeakIndex = nextPeak(index, values, threshold);
					
					float seriesPeakDistance = seriesPeakIndex - index;
					
					if (seriesPeakDistance <= 0)
						seriesPeakDistance += values.size();
					
					if (seriesPeakDistance < peakDistance) {
						peakIndex = seriesPeakIndex;
						peakValue = values.get(Math.round(peakIndex));
						peakDistance = seriesPeakDistance;
					}
				}
			}

			// Call the nextPeak method to find the next peak and update the chart
			if (peakIndex != Float.POSITIVE_INFINITY) {
				System.out.println("Next peak is at index: " + peakIndex);
				marker.setValue(peakIndex);
				marker.setLabel(String.format("Value: %.2f, Derivative: %.2f", peakValue, 0.0f));
			}
		} else {
			super.actionPerformed(e);
		}
	}

	 /**
     * Refreshes the plot after updates.
     */
    private void refreshPlot() {
    	for(int c = 0; c < currentColors.length; ++c) {
    		updateChartColor(currentColors[c], c);
    	}
    }

	@Override
	public void setSlice(int slice) {
		
	}

	@Override
	public void setTimeSlice(int tSlice) {
		
	}

	@Override
	public boolean updateImageExtents() {
		return false;
	}

	@Override
	public boolean updateImages() {
		return false;
	}

	@Override
	public boolean updateImages(boolean flag) {
		return false;
	}

	@Override
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {			
		return false;
	}
	
	public void setChannelSelected(List<Boolean> channelSelected) {
		this.channelSelected = channelSelected;
		refreshPlot();
	}
	
	public void setChannelSelected(int which, boolean selected) {
		channelSelected.set(which, selected);
		refreshPlot();
	}
	

	 /**
     * Sets the LUT for color updates based on selected LUT.
     * @param lut the LUT data structure from which to extract the color information
     */
	public void setLUT(ModelStorageBase lut, int channelIndex) {
		if(lut instanceof ModelLUT) {
			ModelLUT lut2 = (ModelLUT) lut;
			int[] extents = lut2.getExtents();// Get the dimensions of the LUT
			Color c = lut2.getColor(extents[1]-1);// Fetch the color at the last index of the LUT
			updateChartColor(c, channelIndex);
		}
	}

	/**
     * Updates the chart color and applies a gradient based on the selected color channel.
     * @param color the color to which the chart elements will be updated.
     */
	private void updateChartColor(Color color, int channelIndex) {
		currentColors[channelIndex] = color;
	    XYPlot plot = selectionChart.getXYPlot();
		GradientPaint gradientPaint = createGradientPaint(color);
	    XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) plot.getRenderer();
	    renderer.setSeriesPaint(channelIndex * 2, gradientPaint);
	    if (channelSelected.size() > channelIndex)
	    	renderer.setSeriesVisible(channelIndex * 2, channelSelected.get(channelIndex));
	    renderer.setSeriesVisible(channelIndex * 2 + 1, false);
	    plot.setRenderer(renderer);
	    repaint();
	}
  
	/**
     * Creates a gradient paint from black to the specified color.
     * @param color the target end color of the gradient.
     * @return GradientPaint object that specifies the gradient properties.
     */
	private GradientPaint createGradientPaint(Color color) {
	    float height = (float) getBounds().getHeight();// Determine the height of the chart for the gradient scale
	    return new GradientPaint(0, height, Color.BLACK, 0, 0, color, true);// Create a gradient that transitions vertically
	}

	/**
     * Sets up the renderer with a gradient paint.
     * @param plot the plot to which the renderer will be set.
     */
	private void setupRendererWithGradient(XYPlot plot, int channelIndex) {
	    GradientPaint gradientPaint = createGradientPaint(currentColors[channelIndex]);
	    if (gradientPaint != null) {
	        XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer();
	        renderer.setSeriesPaint(channelIndex, gradientPaint);
	        renderer.setSeriesStroke(channelIndex, new BasicStroke(2.0f));
	        plot.setRenderer(renderer);
	    }
	}

	public void setChannelNames(String[] channelNames) {
		this.channelNames = channelNames;
		this.currentColors = IntStream
				.range(0, channelNames.length)
				.mapToObj(i -> Color.YELLOW)
				.toArray(Color[]::new);
	}
}
