package gov.nih.mipav.view.graphVisualization;

import hypergraph.graphApi.Node;
import hypergraph.hyperbolic.ModelPoint;
import hypergraph.visualnet.DefaultNodeRenderer;
import hypergraph.visualnet.GraphPanel;

import java.awt.Color;
import java.awt.Font;

public class MipavNodeRenderer extends DefaultNodeRenderer {

    /***/
	private static final long serialVersionUID = 1686126148353845051L;

	@Override
	public void configure(GraphPanel c, ModelPoint mp, Node node) {
        super.configure(c, mp, node );
        if (getBackground() == null) {
            Color back = c.getBackground();
            setBackground(new Color(back.getRed(), back.getGreen(), back.getBlue(), 224));
        }
        setFont(getFont().deriveFont(Font.BOLD));
    }
}
