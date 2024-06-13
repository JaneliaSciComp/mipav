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

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;
import java.util.List;

public class SelectionChartPanel extends ChartPanel implements MarkerChangeListener {


/**
 * CustomChartPanel is a custom extension of the ChartPanel class from JFreeChart
 * used to display and interact with a dynamic XY line chart. It includes custom
 * behaviors for handling marker changes and mouse drag events.
 */
	private static final long serialVersionUID = 1L;
	private JFreeChart selectionChart;
	private List<Vector3f> chart3DPoints;
	private PlugInDialogVolumeRenderDualJanelia parent;

	public SelectionChartPanel(List<Float> values, String title, PlugInDialogVolumeRenderDualJanelia parent) {
		super(null);
		this.selectionChart = createChart(values, title, this);
		this.parent = parent;
		this.setChart(selectionChart);
		setPreferredSize(new Dimension(100,100));//  plot gets larger vertically and horizontally if remove this line
		initialize();

	}
	
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

				// Determine the corresponding Y-value by finding the nearest index
				XYSeriesCollection dataset = (XYSeriesCollection) plot.getDataset();
				XYSeries series = dataset.getSeries(0);

				int index = findNearestXIndex(series, x);
				double y = series.getY(index).doubleValue();

				// Update the marker's value and label on the chart
				ValueMarker marker = (ValueMarker) plot.getDomainMarkers(Layer.FOREGROUND).iterator().next();
				marker.setValue(x);
				marker.setLabel(String.format("Value: %.2f", y));
			
				marker.setLabelFont(new Font("Serif", Font.BOLD, 14));
				marker.setStroke(new BasicStroke(2.0f)); 

				// Redraw the chart to reflect changes
				repaint();
				e.consume();
			}
		});
	}
	
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

	public void updateChart(List<Float> values, String title) {
		this.selectionChart = createChart(values, title, this);
		setChart(this.selectionChart);
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
	private static JFreeChart createChart(List<Float> values, String title, MarkerChangeListener markerListener) {
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

		// Create the chart
		JFreeChart chart = ChartFactory.createXYLineChart(title, "Index", "Value", dataset, PlotOrientation.VERTICAL,
				true, true, false);

		XYPlot plot = chart.getXYPlot();
		plot.setBackgroundPaint(Color.GRAY);
		plot.setDomainGridlinePaint(Color.DARK_GRAY);
		plot.setRangeGridlinePaint(Color.DARK_GRAY); 
		
		XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer();
		renderer.setSeriesPaint(0, Color.YELLOW);
		renderer.setSeriesStroke(0, new BasicStroke(2.0f));
		plot.setRenderer(renderer);

		ValueMarker marker = new ValueMarker(maxIndex);
		marker.setPaint(Color.CYAN);
		marker.setLabel("Max Value: " + maxValue);

		marker.setLabelFont(new Font("Serif", Font.BOLD, 14)); 
		marker.setLabelPaint(Color.WHITE);

		marker.setLabelAnchor(RectangleAnchor.CENTER);
		marker.setLabelTextAnchor(TextAnchor.CENTER_LEFT);
		marker.setStroke(new BasicStroke(2.0f)); 
		plot.addDomainMarker(marker);
		
		
		marker.addChangeListener(markerListener);

		return chart;
	}

	public void setChart3DPoints(List<Vector3f> points) {
		this.chart3DPoints = points;
	}
}
