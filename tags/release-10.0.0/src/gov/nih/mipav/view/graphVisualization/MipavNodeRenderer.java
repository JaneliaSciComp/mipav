package gov.nih.mipav.view.graphVisualization;

import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Graph;
import hypergraph.graphApi.Node;
import hypergraph.hyperbolic.ModelPoint;
import hypergraph.visualnet.DefaultNodeRenderer;
import hypergraph.visualnet.GraphPanel;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.ActionEvent;

import javax.swing.Icon;

/**
 * Overrides how the Nodes are displayed in the HyperGraph. Add/modify to change the Node display for MIPAV graphs. 
 */
public class MipavNodeRenderer extends DefaultNodeRenderer {

    /** generated serial id */
	private static final long serialVersionUID = 1686126148353845051L;

	@Override
	public void configure(GraphPanel c, ModelPoint mp, Node node) {
        //super.configure(c, mp, node );

        
        
    	graphPanel = c;
		this.node = node;
		if (node == null)
			getTextRenderer().configure(c, null, null);
		else {
			int iconWidth = 0;
			int iconHeight = 0;
			int expanderWidth = 0;
			// initializing call to configure of the text renderer
			Graph graph = ((GraphPanel) c).getGraph();
			
			AttributeManager attrMgr = graph.getAttributeManager();
			String action = (String)attrMgr.getAttribute( "ACTION", node );
			
			if ( action != null && (!action.trim().equals("")) ){
				getTextRenderer().configure(c, mp, node.getLabel()+"*");
			}else {
				getTextRenderer().configure(c, mp, node.getLabel());
			}
			Component textComponent = getTextRenderer().getComponent();
			// reading some attributes for the node
			

			//Color textColour = (Color) attrMgr.getAttribute(GraphPanel.NODE_FOREGROUND, node);
			//if (textColour != null)
				//getTextRenderer().setColor(textColour);
			Color fillColour = (Color) attrMgr.getAttribute(GraphPanel.NODE_BACKGROUND, node);
			getTextRenderer().setBackground(fillColour);
			setBackground(fillColour);
			if (((GraphPanel) c).getSelectionModel().isElementSelected(node))
				setFont(getFont().deriveFont(Font.BOLD));
			// adjust the size to have space for the +/- sign to expand if necessary
			if (graphPanel.hasExpander(node))
				expanderWidth = 10;
			// adjust the size to have space for the icon if necessary
			icon = (Icon) attrMgr.getAttribute(GraphPanel.NODE_ICON, node);
			if (icon != null) {
				iconHeight = icon.getIconHeight();
				iconWidth = icon.getIconWidth();
			}
			setSize(iconWidth + textComponent.getWidth() + expanderWidth,
					Math.max(iconHeight, textComponent.getHeight()));
			int borderx = getWidth() - textComponent.getWidth();
			int bordery = getHeight() - textComponent.getHeight();
			setLocation(textComponent.getX() - borderx / 2, textComponent.getY() - bordery / 2);
			textComponent.setLocation(getX() + iconWidth, getY() + (getHeight() - textComponent.getHeight()) / 2);
 			if (((GraphPanel) c).getHoverElement() == node) {
 				/*if (getBackground() == null) {
 					Color back = c.getBackground();
 					setBackground(new Color(back.getRed(), back.getGreen(), back.getBlue(), 224));
 				}*/
 				//setBackground(Color.white);
 				//getTextRenderer().setColor(Color.blue);

				setFont(getFont().deriveFont(Font.BOLD));
				if ( action != null && (!action.trim().equals("")) ){
					c.setCursor(new Cursor(Cursor.HAND_CURSOR));
				}else {
					c.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				}
				
				String kNotes = (String)attrMgr.getAttribute( "ANNOTATION", node );	
				
				
				if(kNotes != null && !(kNotes.trim().equals(""))) {

					c.setToolTipText(kNotes);
			
				}else {
					c.setToolTipText(null);
				}
				
				
				
				Color textColour = (Color) attrMgr.getAttribute(GraphPanel.NODE_FOREGROUND, node);
				if (textColour != null) {

					Color hoverColour = new Color(textColour.getRed(), textColour.getGreen() , textColour.getBlue(), 255);
					getTextRenderer().setColor(hoverColour);
					

				}
				
				
				
 			}else {
 				c.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
 				
 				Color cl = (Color) attrMgr.getAttribute(GraphPanel.NODE_FOREGROUND, node);
 				Color textColour = new Color(cl.getRed(), cl.getGreen(), cl.getBlue(), 180);
 			
				if (textColour != null) {
					
					getTextRenderer().setColor(textColour);
				}
				
				c.setToolTipText(null);
				
				
 			}
		}
        
        
        

        
        
        
        // This sets each node so that it is highlighted, with the node clearly distinguishable from the
        // intersecting edge lines.
        if (getBackground() == null) {
            Color back = c.getBackground();
            setBackground(new Color(back.getRed(), back.getGreen(), back.getBlue(), 224));
        }
        if ( getFont() == null )
        {
        	setFont(c.getFont().deriveFont(Font.BOLD));
        }
        else
        {
        	
        	setFont(getFont().deriveFont(Font.BOLD));
        	
        	/*setFont(getFont().deriveFont(Font.BOLD));
        	//System.out.println(node.getElementType());
        	

        	//System.out.println(label);
        	
        	//System.out.println();
        	
        	if ( c.getGraph().getEdges(node).size() == 1 &&  node.getLabel() != null ){
        		setFont(getFont().deriveFont(Font.ITALIC));
			}else {
				setFont(getFont().deriveFont(Font.BOLD));
			}
        	*/
        	
        	
        }
        
        
        
        
        
        
  
        
    }
}
