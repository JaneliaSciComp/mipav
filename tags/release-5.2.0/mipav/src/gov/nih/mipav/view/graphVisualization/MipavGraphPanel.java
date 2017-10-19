package gov.nih.mipav.view.graphVisualization;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.*;
import java.util.Enumeration;
import java.util.Iterator;

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import hypergraph.visualnet.GraphLayoutModel;
import hypergraph.visualnet.GraphPanel;
import hypergraph.visualnet.NodeRenderer;
import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Edge;
import hypergraph.graphApi.Element;
import hypergraph.graphApi.Graph;
import hypergraph.graphApi.Node;
import hypergraph.visualnet.DefaultNodeRenderer;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.ViewUserInterface;

public class MipavGraphPanel extends GraphPanel implements ActionListener {

    /** */
	private static final long serialVersionUID = -5403703931672321741L;
	private ViewJFrameImage m_kImageFrame = null;

	protected ViewJColorChooser colorChooser;
	protected Node pickedNode;
	
    public MipavGraphPanel( Graph kGraph, ViewJFrameImage kImageFrame )
    {
        super(kGraph);
        m_kImageFrame = kImageFrame;
        getEdgeRenderer().setLabelVisible(true);
        setSmallLogo(null);
        setLogo(null);
    }
    
    
	public void mouseClicked(MouseEvent e) {
		if ( e.getButton() == MouseEvent.BUTTON3 )
		{
	    	pickedNode = null;
			setHoverElement(null, false);
			Element element = getElement(e.getPoint());
			if ( element == null )
			{
				System.err.println( "Right click background" );
				popupBackground(e);
			}
			else if (element.getElementType() == Element.NODE_ELEMENT) {
				popupNode( e, (Node)element );
				System.err.println( "Right click node" );
			}
			else if (element.getElementType() == Element.EDGE_ELEMENT) {
				System.err.println( "Right click edge" );
			}
		}
		else
		{
			super.mouseClicked(e);
		}
	}

    /** Called if the node is clicked.
     * @param iClickCount The number of clicks on the node.
     * @param kNode The node that has been clicked on.
     */
    public void nodeClicked(int iClickCount, Node kNode)
    {
        super.nodeClicked(iClickCount, kNode);
        /*
        System.err.println( kNode.getLabel() );
		AttributeManager attrMgr = getGraph().getAttributeManager();
		Iterator elements = attrMgr.getAttributeNames().iterator();
		while ( elements.hasNext() )
		{
			String kAttr = (String)elements.next();
    		if ( attrMgr.getAttribute( kAttr, kNode) != null )
    		{
    			System.err.println( kAttr + " " + 
    					attrMgr.getAttribute( kAttr, kNode) );
    		}
		}
         */
        if ( (iClickCount >= 2) && (m_kImageFrame != null) && (kNode != null) )
        {
        	if ( getGraph().getEdges(kNode).size() == 1 &&  kNode.getLabel() != null )
            {
                m_kImageFrame.actionPerformed( new ActionEvent( kNode, 0, kNode.getLabel() ) );
            }
        }
    }

    public void refreshText()
    {
    	setTextRenderer(null);
        if ( getNodeRenderer() != null )
        {
            ((DefaultNodeRenderer)getNodeRenderer()).setTextRenderer(null);
        }
    }


    private void popupBackground(MouseEvent mouseEvent) {

        // build VOI intensity popup menu
        JPopupMenu popupMenu = new JPopupMenu();

        JMenuItem menuItem = new JMenuItem("Center root node");
        popupMenu.add(menuItem);
        menuItem.addActionListener(this);
        menuItem.setActionCommand("Center root node" );
        menuItem = new JMenuItem("Set background color");
        popupMenu.add(menuItem);
        menuItem.addActionListener(this);
        menuItem.setActionCommand("Set background color" );
        
        popupMenu.show(this, mouseEvent.getX(), mouseEvent.getY() );
    }


    private void popupNode(MouseEvent mouseEvent, Node node) {
    	pickedNode = node;
    	
        // build VOI intensity popup menu
        JPopupMenu popupMenu = new JPopupMenu();

        JMenuItem menuItem = new JMenuItem("Set node color");
        popupMenu.add(menuItem);
        menuItem.addActionListener(this);
        menuItem.setActionCommand("Set node color" );
        
        menuItem = new JMenuItem("Set tree level color");
        popupMenu.add(menuItem);
        menuItem.addActionListener(this);
        menuItem.setActionCommand("Set tree level color" );
        
        popupMenu.show(this, mouseEvent.getX(), mouseEvent.getY() );
    }


	@Override
	public void actionPerformed(ActionEvent e) {
		final String command = e.getActionCommand();

		if (command.equalsIgnoreCase("Center root node")) {   

			Node root = findRoot();
			if ( root != null )
			{
				centerNode(root);
			}
		}
		if (command.equalsIgnoreCase("Set background color")) {     
			colorChooser = new ViewJColorChooser(null, "Pick color", new ActionListener()
			{ // OKAY listener
				public void actionPerformed(final ActionEvent ae) {
					String kColorString = new String( "#" + Integer.toHexString(colorChooser.getColor().getRGB()).substring(2) );
					getPropertyManager().setProperty( "hypergraph.hyperbolic.background.color",
							kColorString );
					refreshProperties();
				}
			}, new ActionListener() { // CANCEL listener
				public void actionPerformed(final ActionEvent a) {}
			});
		}
		if (command.equalsIgnoreCase("Set node color")) {     
			colorChooser = new ViewJColorChooser(null, "Pick color", new ActionListener()
			{ // OKAY listener
				public void actionPerformed(final ActionEvent ae) {
					if ( pickedNode != null )
					{
						AttributeManager attrMgr = getGraph().getAttributeManager();
						attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNode, colorChooser.getColor() );
					}
				}
			}, new ActionListener() { // CANCEL listener
				public void actionPerformed(final ActionEvent a) {}
			});
		}
		if (command.equalsIgnoreCase("Set tree level color")) {     
			colorChooser = new ViewJColorChooser(null, "Pick color", new ActionListener()
			{ // OKAY listener
				public void actionPerformed(final ActionEvent ae) {
					if ( pickedNode != null )
					{
						AttributeManager attrMgr = getGraph().getAttributeManager();
						Integer level = (Integer)attrMgr.getAttribute( "TreeLevel", pickedNode );
						if ( level != null )
						{
							if ( setNodeColor( level.intValue(), colorChooser.getColor() ) )
							{
								return;
							}
						}
						
						Node root = (Node)attrMgr.getAttribute( AttributeManager.GRAPH_ROOT, pickedNode );
						if ( root == null )
						{
							root = findRoot();
						}
						if ( root != null )
						{
							if ( level == null )
							{
								level = findLevel( root, pickedNode, 0 );
							}
							if ( level != null )
							{
								setNodeColor( root, 0, level.intValue(), colorChooser.getColor() );
							}
						}
					}
				}
			}, new ActionListener() { // CANCEL listener
				public void actionPerformed(final ActionEvent a) {}
			});
		}
		
	}

	public Node findRoot()
	{
		Iterator iterator = getGraph().getNodes().iterator();
		Node minNode = null; // the node with the smallest incoming degree so far.
		Node next = null; // the next node to be tested.
		int minIncomingDegree = -1; // the current in degree
		while (iterator.hasNext()) {
			next = (Node) iterator.next();
			int inDegree = 0;
			for (Iterator iter = getGraph().getEdges(next).iterator(); iter.hasNext();) {
				Edge edge = (Edge) iter.next();
				if (edge.getTarget().equals(next))
					inDegree++;
			}
			if (minIncomingDegree < 0
					|| minIncomingDegree > inDegree) {
				minNode = next; // next is either the first node or better than all before
				minIncomingDegree = inDegree;
			}
			if (minIncomingDegree == 0)
				break; // the degree can not be smaller than 0, we can stop here.
		}
		if ( minIncomingDegree == 0 )
		{
			return minNode;
		}
		return null;
	}


	public Integer findLevel(Node root, Node target, int iLevel)
	{
		for (Iterator iter = getGraph().getEdges(root).iterator(); iter.hasNext();) {
			Edge edge = (Edge) iter.next();
			if (edge.getSource().equals(root) && edge.getTarget().equals(target) )
			{
				return new Integer(iLevel + 1);
			}
		}
		for (Iterator iter = getGraph().getEdges(root).iterator(); iter.hasNext();) {
			Edge edge = (Edge) iter.next();
			if (edge.getSource().equals(root) )
			{
				Integer result = findLevel( edge.getTarget(), target, iLevel+1 );
				if ( result != null )
				{
					return result;
				}
			}
		}
		return null;
	}

	
	public boolean setNodeColor( int iTreeLevel, Color kColor )
	{
		AttributeManager attrMgr = getGraph().getAttributeManager();
		Iterator iterator = getGraph().getNodes().iterator();
		while( iterator.hasNext() )
		{
			Node kNode = (Node)iterator.next();
			Integer level = (Integer)attrMgr.getAttribute( "TreeLevel", kNode );
			if ( level == null )
			{
				return false;
			}
			if ( level.intValue() == iTreeLevel )
			{
				attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, kNode, kColor );				
			}
		}
		return true;
	}
	
	public void setNodeColor( Node root, int iTreeLevel, int iTargetLevel, Color kColor )
	{
		if ( iTreeLevel == iTargetLevel )
		{
			AttributeManager attrMgr = getGraph().getAttributeManager();
			attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, root, kColor );		
			return;
		}
		Iterator iter = getGraph().getEdges(root).iterator();
		while ( iter.hasNext() )
		{
			Edge edge = (Edge) iter.next();
			if ( edge.getSource().equals(root) )
			{
				Node target = edge.getTarget();
				setNodeColor( target, iTreeLevel+1, iTargetLevel, kColor );
			}
		}		
	}

}