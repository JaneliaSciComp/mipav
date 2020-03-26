package gov.nih.mipav.view.graphVisualization;

import java.awt.Color;

import hypergraph.visualnet.ArrowLineRenderer;
import hypergraph.visualnet.DefaultEdgeRenderer;
import hypergraph.visualnet.GraphPanel;
import java.awt.BasicStroke;

import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Edge;
import hypergraph.hyperbolic.Isometry;
import hypergraph.hyperbolic.LineRenderer;
import hypergraph.hyperbolic.ModelPoint;
import hypergraph.hyperbolic.TextRenderer;

public class MipavEdgeRenderer extends DefaultEdgeRenderer {
	
	/** Indicates whether the label of the edge has to be shown or not. */
	private		boolean			labelVisible;
	
	
	public void setLabelVisible(boolean vis) { labelVisible = vis; }

	/** {@inheritDoc} */
	public boolean isLabelVisible() { return labelVisible; }
	
	public void configure(GraphPanel mp, Edge edge) {
		graphPanel = mp;
		setBounds(0, 0, mp.getWidth(), mp.getHeight());
		AttributeManager attrMgr = mp.getGraph().getAttributeManager();
		ModelPoint mp1 = mp.getGraphLayout().getGraphLayoutModel().getNodePosition(edge.getSource());
		ModelPoint mp2 = mp.getGraphLayout().getGraphLayoutModel().getNodePosition(edge.getTarget());
		if (mp1 != null && mp2 != null) {
			getLineRenderer().configure(mp, mp1, mp2);
			if (labelVisible) {
				ModelPoint center = (ModelPoint) mp1.clone();
				Isometry isom = mp.getModel().getTranslation(mp1, mp2, 0.5);
				isom.apply(center);
				getLabelRenderer().configure(mp, center, edge.getLabel());
				getLabelRenderer().setBackground(null);
				Color colour = null;
				if (attrMgr != null)
					colour = (Color) attrMgr.getAttribute(GraphPanel.EDGE_TEXTCOLOR, edge);
				if (colour != null)
					getLabelRenderer().setColor(colour);
			}
			if (getLineRenderer() instanceof ArrowLineRenderer)
				((ArrowLineRenderer) getLineRenderer()).setShowArrows(edge.isDirected());
		}
		if (attrMgr != null) {
			Color colour = null;
			colour = (Color) attrMgr.getAttribute(GraphPanel.EDGE_LINECOLOR, edge);
			if (colour != null) {
				Color c1 = null;
				if( edge.getSource().equals(mp.getHoverElement()) ||
					edge.getTarget().equals(mp.getHoverElement()) ||
					edge.equals(mp.getHoverElement())) {
					//colour = colour.darker();
					c1 = new Color(colour.getRed(), colour.getGreen(), colour.getBlue(), 255);
					getLineRenderer().setColor(c1);
				}else {
					c1 = new Color(colour.getRed(), colour.getGreen(), colour.getBlue(), 120);
					getLineRenderer().setColor(c1);
				}
				
			}
			
			
			
			float[] stroke = (float[]) attrMgr.getAttribute(GraphPanel.EDGE_STROKE, edge);
			float lineWidth = ((Float) attrMgr.getAttribute(GraphPanel.EDGE_LINEWIDTH, edge)).floatValue();
			if (stroke != null && stroke.length > 0)
				lineStroke = new BasicStroke(lineWidth, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 10.0f, stroke , 0.0f);
			else if (stroke != null && stroke.length == 0)
				lineStroke = null;
			else
				lineStroke = new BasicStroke(lineWidth, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL);
		}
	}

}
