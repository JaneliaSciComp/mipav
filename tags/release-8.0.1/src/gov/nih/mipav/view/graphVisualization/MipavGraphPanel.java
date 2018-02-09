package gov.nih.mipav.view.graphVisualization;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.components.WidgetFactory;
import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Edge;
import hypergraph.graphApi.Element;
import hypergraph.graphApi.Graph;
import hypergraph.graphApi.Node;
import hypergraph.hyperbolic.ModelPoint;
import hypergraph.visualnet.DefaultNodeRenderer;
import hypergraph.visualnet.GraphLayoutModel;
import hypergraph.visualnet.GraphPanel;
import hypergraph.visualnet.NodeRenderer;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Vector;


import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.Timer;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * Displays the Mipav HyperGraph.
 */
public class MipavGraphPanel extends GraphPanel implements ActionListener {

	/** generated serial id */
	private static final long serialVersionUID = -5403703931672321741L;
	/** The ViewJFrameImage connect to this Graph, may be null. */
	private ViewJFrameImage m_kImageFrame = null;

	/** ColorChooser for changing graph colors. */
	protected ViewJColorChooser colorChooser, nodeColorChooser, treeLevelColorChooser;
	/** Current Node to color. */
	protected Node colorNode;
	/** Current selected Node. */
	protected Node pickedNode;
	/** previously selected Node. */
	protected Node pickedNodePrev;
	/** Current selected Edge. */
	protected Edge pickedEdge;
	/** Last command, used for determining the action the color chooser is associated with. */
	private String m_kLastCommand;
	/** If there is no Root node, this is the Node with the smallest number of parent nodes, 
	 * and serves as a default Root for the Graph. */
	private Node m_kMinDegree = null;
	/** Default color to highlight the selected nodes on control- mouse-click: */
	private Color pickedColor = Color.red;
	
	private JDialogHyperGraph ownerDialog;
	
	private PropertiesDialog propertiesDialog;
	
	private JDialogAddNode addNodeDialog;
	
	private JDialogAction actionDialog;
	
	private boolean addNode = false;
	
	private JCheckBox treeCheckBox;
	
	private Graph kGraph;

	private Image logo;
	
	private Node 			lastMouseClickNode;
	
	private boolean isBGImageShowing = false;
	
	private Timer clickTimer;
	
	private Image image, newImage;
	

	/**
	 * Creates the GraphPanel display.
	 * @param kGraph Graph to display.
	 * @param kImageFrame ViewJFrameImage (may be null).
	 */
	public MipavGraphPanel( Graph kGraph, ViewJFrameImage kImageFrame, JDialogHyperGraph ownerDialog )
	{
		super(kGraph);
		this.kGraph = kGraph;
		m_kImageFrame = kImageFrame;
		this.ownerDialog = ownerDialog;
		setNodeRenderer(new MipavNodeRenderer());
		setEdgeRenderer(new MipavEdgeRenderer());
		getEdgeRenderer().setLabelVisible(true);
		try {
			image = MipavUtil.getIconImage("oval4.jpg");
		}catch(FileNotFoundException ex) {
			ex.printStackTrace();
		}
		setSmallLogo(null);
		setLogo(null);
	}


	@Override
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if ( command.equalsIgnoreCase("OK") )
		{
			if ( m_kLastCommand.equalsIgnoreCase("Set background color" ) )
			{
				setLogo(null);
				String kColorString = new String( "#" + Integer.toHexString(colorChooser.getColor().getRGB()).substring(2) );
				getPropertyManager().setProperty( "hypergraph.hyperbolic.background.color",kColorString );
				refreshProperties();
				savePreferences();
				isBGImageShowing = false;
				repaint();
			}
			if (m_kLastCommand.equalsIgnoreCase("Set node color")) {
				pickedNode = colorNode;
				/*if ( pickedNode != null )
				{
					AttributeManager attrMgr = getGraph().getAttributeManager();
					attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNode, nodeColorChooser.getColor() );
				}
				if(propertiesDialog != null) {
					propertiesDialog.dispose();
					propertiesDialog = null;
				}
				repaint();*/
				setProperties();
				repaint();
			}
			if (m_kLastCommand.equalsIgnoreCase("Set tree level color")) { 
			    pickedNode = colorNode;
				if ( pickedNode != null )
				{
					AttributeManager attrMgr = getGraph().getAttributeManager();
					Integer level = (Integer)attrMgr.getAttribute( "TreeLevel", pickedNode );
					if ( level != null )
					{
						
						if ( setNodeColor( level.intValue(), treeLevelColorChooser.getColor() ) )
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
							setNodeColor( root, 0, level.intValue(), treeLevelColorChooser.getColor() );
						}
					}
					else
					{
						MipavUtil.displayError( "Must specify root node." );
					}
				}

				if(propertiesDialog != null) {
					propertiesDialog.dispose();
					propertiesDialog = null;
				}
				repaint();
				//setProperties();
				//repaint();
			}
		}
		if (command.equalsIgnoreCase("Center root node")) {   
			centerRootNode();
		}
		if (command.equalsIgnoreCase("Increase Font Size")) {   
			increaseTextSize(true);
			savePreferences();
		}
		if (command.equalsIgnoreCase("Decrease Font Size")) {   
			increaseTextSize(false);
			savePreferences();
		}

		m_kLastCommand = new String(command);
		if (command.equalsIgnoreCase("Set background color")) {     
			colorChooser = new ViewJColorChooser(null, "Pick color", this, this );
		}
		if (command.equalsIgnoreCase("Set background image")) {  

			Image image = null;
			/*try {
			    // Read from a file
			    File file = new File("C:\\nish\\oval4.jpg");
			    image = ImageIO.read(file);

			    // Read from an input stream
			    InputStream is = new BufferedInputStream(
			        new FileInputStream("C:\\nish\\oval4.jpg"));
			    image = ImageIO.read(is);
			}catch(Exception e) {
				e.printStackTrace();
			}*/
			try {
				image = MipavUtil.getIconImage("oval4.jpg");
			}catch(FileNotFoundException ex) {
				ex.printStackTrace();
			}
			

			int width = this.getSize().width;
			int height = this.getSize().height;
			Image newImage = image.getScaledInstance(width, height, Image.SCALE_DEFAULT);
			

			
			setLogo(newImage);
			Color color = new Color(32,32,126);
			String kColorString = new String( "#" + Integer.toHexString(color.getRGB()).substring(2) );
			getPropertyManager().setProperty( "hypergraph.hyperbolic.background.color",kColorString );
			
			refreshProperties();
			savePreferences();
			isBGImageShowing = true;
			repaint();
		}
		if (command.equalsIgnoreCase("Set node color")) {     
			colorNode = pickedNode;
			colorChooser = new ViewJColorChooser(null, "Pick color", this, this );

		}
		if (command.equalsIgnoreCase("Set tree level color")) {  
			colorNode = pickedNode;   
			colorChooser = new ViewJColorChooser(null, "Pick color", this, this );
		}
		if (command.equalsIgnoreCase("Delete edge")) {     
			if ( pickedEdge != null )
			{
				Node target = pickedEdge.getTarget();
				// Iterate over the edges connected to the target node at the end of the picked edge:
				Iterator iter = getGraph().getEdges(target).iterator();
				boolean otherParents = false;
				while (iter.hasNext()) {
					Edge edge = (Edge) iter.next();
					if ( edge != pickedEdge )
					{
						Node source = edge.getSource();
						if ( source != target )
						{
							// target has other parent edges, only delete edge:
							otherParents = true;
						}
					}
				}
				if ( !otherParents )
				{
					// the target node attached to the picked edge
					// has no other incoming edges, delete the node so
					// it is not orphaned:
					deleteNode( target );
				}
				else
				{
					// the target node is connected to the rest of the
					// graph with another incoming edge, it will not
					// be orphaned when the selected edge is deleted, so
					// just delete the selected edge:
					getGraph().removeElement(pickedEdge);
				}
			}
		}
		if (command.equalsIgnoreCase("Add child node")) {  
			// Launch add node dialog:
			//new JDialogAddNode(this, null, true);

			addNode = true;
			propertiesDialog = new PropertiesDialog(ownerDialog, this,addNode);
		}
		if (command.equalsIgnoreCase("Delete node")) {
			deleteNode(pickedNode);
		}
		if (command.equals("notesNode")) {
			 //pickedNode = colorNode;
			setProperties();
		}
		if(command.equals("actionNode")) {
			setProperties();
		}
		if (command.equalsIgnoreCase("Link nodes")) {
			if ( (pickedNodePrev != null) && (pickedNodePrev != pickedNode) )
			{
				synchronized ( getGraph() ) {
					getGraph().createEdge( pickedNodePrev, pickedNode );
				}
				// Reset the selected nodes to their original colors:
				AttributeManager attrMgr = getGraph().getAttributeManager();			
				Color original = (Color)attrMgr.getAttribute( "OriginalColor", pickedNode );	
				attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNode, original );	
				original = (Color)attrMgr.getAttribute( "OriginalColor", pickedNodePrev );	
				attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNodePrev, original );
				pickedNodePrev = null;
				pickedNode = null;
			}
		}
		if(command.equalsIgnoreCase("Properties") && (pickedNode != null)) {
			colorNode = pickedNode;
			addNode = false;
			propertiesDialog = new PropertiesDialog(ownerDialog, this, addNode);

		}
		/*if ( command.equalsIgnoreCase("Edit Notes") && (pickedNode != null)) {
			// launch the add node/ edit notes dialog, pass in the current Annotation for the 
			// selected node:
			AttributeManager attrMgr = getGraph().getAttributeManager();	
			String kNotes = (String)attrMgr.getAttribute( "ANNOTATION", pickedNode );	
			new JDialogAddNode(this, kNotes, false);
		}*/
		if ( command.equalsIgnoreCase("Set as root")) {
			AttributeManager attrMgr = getGraph().getAttributeManager();		
			attrMgr.setAttribute( AttributeManager.GRAPH_ROOT, pickedNode, pickedNode );
			Iterator iterator = getGraph().getNodes().iterator();
			while (iterator.hasNext()) {
				Node next = (Node) iterator.next();
				attrMgr.setAttribute( AttributeManager.GRAPH_ROOT, next, pickedNode );
			}
		}
		if ( command.equalsIgnoreCase("cancel")) {
			if(propertiesDialog != null) {
					propertiesDialog.dispose();
					propertiesDialog = null;
				}
			
		}
	}
	
	
	 
	public void setProperties() {
		//first do the add node/edit node name/edit node notes part
		String notes = null;
        if(addNodeDialog.getNoteField().getText().length() > 0)
        {
        	notes = addNodeDialog.getNoteField().getText();
        }
        if ( addNode )
        {
        	// add a new node to the graph:
        	addNode(addNodeDialog.getNameField().getText().trim(), notes);
        }
        else{
        	// pass in the edited notes field:

        	if ( notes != null ) {
        		editNotes(notes);
        	}
        	if(pickedNode != null) {
        		pickedNode.setLabel(addNodeDialog.getNameField().getText().trim());
        	}
        }
        
        
        //edit action
        String action = actionDialog.getActionField().getText();

        if(action != null) {
        	editAction(action.trim());
        }
        
        
        
        
        //now do node color
        if ( pickedNode != null )
		{
			AttributeManager attrMgr = getGraph().getAttributeManager();
			attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNode, nodeColorChooser.getColor() );
		}
        if(treeCheckBox.isSelected()) {
        	 //now do tree color
            if ( pickedNode != null )
     		{
     			AttributeManager attrMgr = getGraph().getAttributeManager();
     			Integer level = (Integer)attrMgr.getAttribute( "TreeLevel", pickedNode );
     			if ( level != null )
     			{
     				if ( setNodeColor( level.intValue(), nodeColorChooser.getColor() ) )
     				{
     					if(propertiesDialog != null) {
     						propertiesDialog.dispose();
     						propertiesDialog = null;
     					}
     					repaint();
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
     					setNodeColor( root, 0, level.intValue(), nodeColorChooser.getColor() );
     				}
     			}
     			else
     			{
     				MipavUtil.displayError( "Must specify root node." );
     			}
     		}
        }
        
       
        
        
        if(propertiesDialog != null) {
			propertiesDialog.dispose();
			propertiesDialog = null;
		}
		repaint();
		
		savePreferences();
        
	}
	

	/**
	 * Saves the global graph properties (background color, text sizes) to the
	 * default mipav graph preferences file in the Preferences.getPreferencesDir directory.
	 */
	public void savePreferences()   
	{

		String file = new String("mipavGraphLayout.prop");
		File graphPreferencesFile = new File(Preferences.getPreferencesDir(), file);
		FileWriter kNewFile;
		try {
			kNewFile = new FileWriter( graphPreferencesFile );
			Enumeration properties = getPropertyManager().propertyNames();
			while (properties.hasMoreElements()) {
				String name = (String)properties.nextElement();
				String property = getPropertyManager().getProperty(name).toString();
				if ( (name.indexOf(".class") == -1) && (name.indexOf(";") == -1)  && (property.indexOf(" ") == -1) )
				{
					kNewFile.write( name + "=" + property + "\n" );
				}
			}
			kNewFile.close();
		} catch (IOException e) { }
	}

	/**
	 * Add a new node under the picked node.
	 * @param name The name of the new node.
	 * @param notes Any notes to add to the node's Annotation field.
	 */
	public void addNode( String name, String notes )
	{


		if ( (name != null) && (name.length() > 0) && (pickedNode != null) )
		{

			// Find the root node and the level of the pickedNode:
			AttributeManager attrMgr = getGraph().getAttributeManager();
			Node root = (Node)attrMgr.getAttribute( AttributeManager.GRAPH_ROOT, pickedNode );
			Integer level = (Integer)attrMgr.getAttribute( "TreeLevel", pickedNode );
			if ( root == null )
			{
				root = findMinRoot();
			}
			if ( root != null )
			{
				if ( level == null )
				{
					level = findLevel( root, pickedNode, 0 );
				}
			}

			// Add the new node:
			synchronized ( getGraph() ) {
				Graph tree = getGraph();
				Node newNode = tree.createNode();
				newNode.setLabel( name );
				// set the node color to match others at the same level in the tree
				attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, newNode, getNodeColor(root, level+1) );
				// set the root attribute:
				attrMgr.setAttribute( AttributeManager.GRAPH_ROOT, newNode, root ); 
				attrMgr.setAttribute( "TreeLevel", newNode, level+1 );
				// Set the notes as the annotation for the node:
				attrMgr.setAttribute( "ANNOTATION", newNode, notes );
				// create an edge from the pickedNode to the new node:
				tree.createEdge( pickedNode, newNode );
			}
			pickedNode = null;
		}
		
		repaint();

	}

	/**
	 * Centers the root node in the display.
	 */
	public void centerRootNode()
	{
		Node root = findRoot();
		if ( root != null )
		{
			centerNode(root);
		}
	}


	/**
	 * Modify the Annotation field of the selected node.
	 * @param notes new annotation to add to the picked Node.
	 */
	public void editNotes( String notes )
	{
		if ( pickedNode != null )
		{
			AttributeManager attrMgr = getGraph().getAttributeManager();
			attrMgr.setAttribute( "ANNOTATION", pickedNode, notes );
			
			/*if(propertiesDialog != null) {
				propertiesDialog.dispose();
				propertiesDialog = null;
			}*/
		}
	}
	
	
	
 
	/**
	 * Modify the Annotation field of the selected node.
	 * @param notes new annotation to add to the picked Node.
	 */
	public void editAction( String action )
	{
		
		if ( pickedNode != null )
		{
			AttributeManager attrMgr = getGraph().getAttributeManager();
			attrMgr.setAttribute( "ACTION", pickedNode, action );
			
			String act = (String)attrMgr.getAttribute( "ACTION", pickedNode );
			
			/*if(propertiesDialog != null) {
				propertiesDialog.dispose();
				propertiesDialog = null;
			}*/
		}
	}
	
	
	
	/**
	 * Finds the depth, or level of the target node from the Root node.
	 * @param root Root node of the Graph.
	 * @param target Target Node.
	 * @param iLevel Current depth level.
	 * @return the level of the Target in the Graph.
	 */
	public Integer findLevel(Node root, Node target, int iLevel)
	{
		if ( root == target )
		{
			return iLevel;
		}
		
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

	/**
	 * Returns the node with the smallest number of 'parent' nodes. If the tree has a true Root node,
	 * with zero parents, the Root is returned. Otherwise the node with the smallest number of inputs is returned.
	 * For Graphs without a true Root, there may be more than one MinRoot.
	 * @return
	 */
	public Node findMinRoot()
	{
		findRoot();
		return m_kMinDegree;
	}

	/**
	 * Finds the Root of the tree. If there is no true Root (with zero inputs), returns null.
	 * @return
	 */
	public Node findRoot()
	{
		m_kMinDegree = null;
		Iterator iterator = getGraph().getNodes().iterator();
		Node minNode = null; // the node with the smallest incoming degree so far.
		Node next = null; // the next node to be tested.
		int minIncomingDegree = -1; // the current in degree
		while (iterator.hasNext()) {
			next = (Node) iterator.next();
			//System.err.print( "Checking Node " + next.getLabel() + " " );
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
			//System.err.println( inDegree );
			if (minIncomingDegree == 0)
				break; // the degree can not be smaller than 0, we can stop here.
		}
		m_kMinDegree = minNode;
		if ( minIncomingDegree == 0 )
		{
			return minNode;
		}
		return null;
	}


	/**
	 * Finds the current color for the input level of the tree.
	 * @param root the tree root node.
	 * @param iTreeLevel the level to querery the color of.
	 * @return The node color at the given tree depth.
	 */
	public Color getNodeColor( Node root, int iTreeLevel )
	{
		// First check if the level is set as an attribute of the nodes:
		AttributeManager attrMgr = getGraph().getAttributeManager();
		Iterator iterator = getGraph().getNodes().iterator();
		while( iterator.hasNext() )
		{
			Node kNode = (Node)iterator.next();
			Integer level = (Integer)attrMgr.getAttribute( "TreeLevel", kNode );
			if ( (level != null) && (level.intValue() == iTreeLevel) )
			{
				return (Color)attrMgr.getAttribute( GraphPanel.NODE_FOREGROUND, kNode );				
			}
		}
		// No level attribute was set, so recursively search the tree until the depth is found
		// and return the color:
		Color kReturn = findNodeColor( root, 0, iTreeLevel );
		if ( kReturn == null )
		{
			// there were no nodes at the requested tree depth, return the root node color as default:
			kReturn = (Color)attrMgr.getAttribute( GraphPanel.NODE_FOREGROUND, root );	
		}
		return kReturn;
	}


	/**
	 * Increases or decreases the displayed text size.
	 * @param bBigger when true increases the size, when false decreases the size.
	 */
	public void increaseTextSize( boolean bBigger )
	{
		float[] defaults = new float[]{12,10,8,0};
		for ( int i = 0; i < 4; i++ )
		{			
			float size = (float)getPropertyManager().getDouble("hypergraph.hyperbolic.text.size" + (i+1), new Double(defaults[i])).doubleValue();
			size *= bBigger ? 1.1 : .9;
			getPropertyManager().setProperty( "hypergraph.hyperbolic.text.size" + (i+1), String.valueOf(size) );
		}
		refreshText();
		repaint();
	}


	/* (non-Javadoc)
	 * @see hypergraph.visualnet.GraphPanel#mouseClicked(java.awt.event.MouseEvent)
	 */
	public void mouseClicked(MouseEvent e) {
		final int clickDelay=500; //delay in msec before processing events
		
		
		
		if ( e.getButton() == MouseEvent.BUTTON3 )
		{
			// Popup a menu on right-click:
			pickedNode = null;
			setHoverElement(null, false);
			Element element = getElement(e.getPoint());
			if ( element == null )
			{
				popupBackground(e);
			}
			else if (element.getElementType() == Element.NODE_ELEMENT) {
				popupNode( e, (Node)element );
			}
			else if (element.getElementType() == Element.EDGE_ELEMENT) {
				popupEdge( e, (Edge)element );
			}
		}else {

			resetPicked();
			

			setHoverElement(null, false);
			final Element element = getElement(e.getPoint());

			
			if (element != null && element.getElementType() == Element.NODE_ELEMENT) {
				if (e.getClickCount() == 1) {
					
					AttributeManager attrMgr = getGraph().getAttributeManager();
					String  action = (String)attrMgr.getAttribute( "ACTION", (Node) element );
					
					
					if ( action != null && (!action.trim().equals("")) )
					{
						clickTimer=new Timer(clickDelay, new ActionListener(){
							public void actionPerformed(ActionEvent e){
								//do something for the single click
								nodeClicked(1, (Node) element);
								return;
								
							}
						});
					 clickTimer.setRepeats(false); //after expiring once, stop the timer
					 clickTimer.start();
					}
					
					
					 
					

				}
				if (e.getClickCount() == 2) {
					if(clickTimer != null) {
						clickTimer.stop(); //the single click will not be processed
					}

					nodeClicked(2, (Node) element);
					return;
				}
				NodeRenderer nr = getNodeRenderer();
				GraphLayoutModel glm = getGraphLayout().getGraphLayoutModel();
				nr.configure(this, glm.getNodePosition((Node) element), (Node) element);
			}


			
			
		}
		

	}

	@Override
	public void mouseMoved(MouseEvent e) {
		if ( !e.isControlDown() )
		{
			//System.out.println("mouseMoved:calling resetPicked");
			//resetPicked();
		}
		super.mouseMoved(e);
	}
	
	

	
	
	
	public synchronized PropertiesDialog getPropertiesDialog() {
		return propertiesDialog;
	}


	@Override
	public void mousePressed(MouseEvent e) {
		if ( e.isControlDown() )
		{
			// When the control-key is down, the selected nodes are
			// stored for linking:
			Element element = getHoverElement();
			if ( element instanceof Node )
			{
				if ( element == pickedNode )
				{
					return;
				}
				AttributeManager attrMgr = getGraph().getAttributeManager();
				if ( pickedNodePrev != null  )
				{
					// two nodes were already selected, reset the first node to it's original
					// color and before moving on to the next selected node
					Color original = (Color)attrMgr.getAttribute( "OriginalColor", pickedNodePrev );	
					attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNodePrev, original );	
				}
				// set the previous node:
				pickedNodePrev = pickedNode;
				
				// set the current picked node:
				pickedNode = (Node)element;	
				// save the original color:
				Color original = (Color)attrMgr.getAttribute( "OriginalColor", pickedNode );
				if ( original == null )
				{
					original = (Color)attrMgr.getAttribute( GraphPanel.NODE_FOREGROUND, pickedNode );	
					attrMgr.setAttribute( "OriginalColor", pickedNode, original );	
				}
				// set the picked node picked color:
				attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNode, pickedColor );	
				
				if ( pickedNodePrev != null )
				{
					//System.err.println( pickedNodePrev.getLabel() + " " + pickedNode.getLabel() );
					// save the original color:
					original = (Color)attrMgr.getAttribute( "OriginalColor", pickedNodePrev );
					if ( original == null )
					{
						original = (Color)attrMgr.getAttribute( GraphPanel.NODE_FOREGROUND, pickedNodePrev );	
						attrMgr.setAttribute( "OriginalColor", pickedNodePrev, original );	
					}	
					// set the picked color:
					attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNodePrev, pickedColor );	
				}	
				repaint();
			}
		}
		else if ( !e.isControlDown() )
		{

			resetPicked();
			
			super.mousePressed(e);
		}
	}
	
	@Override
	public void mouseReleased(MouseEvent e) {
		if ( !e.isControlDown() )
		{

			resetPicked();
		}
		super.mouseReleased(e);
	}

	/** Called if the node is clicked.
	 * @param iClickCount The number of clicks on the node.
	 * @param kNode The node that has been clicked on.
	 */
	public void nodeClicked(int iClickCount, Node kNode)
	{
		//super.nodeClicked(iClickCount, kNode);
		
		if (iClickCount == 2) {
			/*lastMouseClickNode = kNode;
			if (lastMouseClickNode != null)
				centerNode(lastMouseClickNode);*/
			centerNode(kNode);
		}
		
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
		if ( (iClickCount == 1) && (m_kImageFrame != null) && (kNode != null) )
		{

			
			AttributeManager attrMgr = getGraph().getAttributeManager();
			String  action = (String)attrMgr.getAttribute( "ACTION", kNode );
			
			
			if ( action != null && (!action.trim().equals("")) )
			{
				
				if(action.startsWith("http")) {
					openURL(action);
				}else {
					m_kImageFrame.actionPerformed( new ActionEvent( kNode, 0, action ) );
				}
				
				
			}/*else {
				if(getGraph().getEdges(kNode).size() == 1) {
					m_kImageFrame.actionPerformed( new ActionEvent( kNode, 0, kNode.getLabel() ) );
				}
			}*/
		}
	}

	/**
	 * Refreshes the text display.
	 */
	public void refreshText()
	{
		setTextRenderer(null);
		if ( getNodeRenderer() != null )
		{
			((DefaultNodeRenderer)getNodeRenderer()).setTextRenderer(null);
		}
	}
	
	/**
	 * Sets the node color for all the nodes at the given level of the Graph. Uses the 
	 * "TreeLevel" node Attribute.
	 * @param iTreeLevel level of the Graph from the root.
	 * @param kColor new color.
	 * @return true on success, false if the "TreeLevel" node attribute is not set.
	 */
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

	/**
	 * Uses recursion to set the node color for all the nodes at a given level of the Graph.
	 * @param root Root Node.
	 * @param iTreeLevel current level in the recursion.
	 * @param iTargetLevel target level.
	 * @param kColor new color.
	 */
	public void setNodeColor( Node root, int iTreeLevel, int iTargetLevel, Color kColor )
	{
		if ( iTreeLevel > iTargetLevel )
		{
			return;
		}
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



	/**
	 * Delete the input node and it's children nodes:
	 * Cannot delete the root node.
	 * @param node
	 */
	private void deleteNode( Node node )
	{
		if ( node == null )
		{
			return;
		}
		AttributeManager attrMgr = getGraph().getAttributeManager();
		Node root = (Node)attrMgr.getAttribute( AttributeManager.GRAPH_ROOT, node );
		if ( root == null )
		{
			root = findMinRoot();
		}
		if ( node == root )
		{
			MipavUtil.displayError( "Cannot delete the root ndoe" );
			return;
		}
		// Add the node to delete to the delete list:
		Vector<Node> deleteList = new Vector<Node>();
		deleteList.add(node);
		// recursively add children nodes to the delete list:
		deleteNode( node, node, root, deleteList );
		
		// delete the nodes on the list:
		for ( int i = deleteList.size() -1; i >= 0; i-- )
		{
			getGraph().removeElement(deleteList.elementAt(i));
		}		
	}

	/**
	 * Recursively deletes children from the tree. The original node to delete is the start node. The
	 * recursion stops when the root node is reached, or if the start node is reached (for example when
	 * it is part of a loop with it's children) or when all the children have been added to the delete list.
	 * @param node current node to delete.
	 * @param start original parent node that triggered the delete.
	 * @param root the root of the graph (cannot be deleted)
	 * @param deleteList the list of nodes to be deleted. Prevents infinite recursion when nodes are in a loop.
	 */
	private void deleteNode( Node node, Node start, Node root, Vector<Node> deleteList )
	{		
		// get all outgoing edges in the original graph.
		Iterator iter = getGraph().getEdges(node).iterator();
		while (iter.hasNext()) {
			Edge edge = (Edge) iter.next();
			if (edge.getSource() != node)
				continue; // only outgoing edges
			Node target = edge.getOtherNode(node);
			// if the target is not the original start node, or the root, or alread on the deleteList:
			if ( (target != start) && (target != root) && !deleteList.contains( target ) )
			{
				deleteList.add(target);
				deleteNode( target, start, root, deleteList );
			}
		}
	}


	/**
	 * Recursively finds the node color at a target level of the tree.
	 * @param node the current node (root of the subtree).
	 * @param iLevel the current recursion level.
	 * @param iTargetLevel the target level of the tree.
	 * @return the color of the Node at the target level.
	 */
	private Color findNodeColor(Node node, int iLevel, int iTargetLevel)
	{
		AttributeManager attrMgr = getGraph().getAttributeManager();
		if ( iLevel == iTargetLevel )
		{
			return (Color)attrMgr.getAttribute( GraphPanel.NODE_FOREGROUND, node );	
		}
		Color kReturn = null;
		for (Iterator iter = getGraph().getEdges(node).iterator(); iter.hasNext();) {
			Edge edge = (Edge) iter.next();
			if (edge.getSource().equals(node) )
			{
				Color kResult = findNodeColor( edge.getTarget(), iLevel+1, iTargetLevel );
				if ( kResult != null )
				{
					kReturn = kResult;
				}
			}
		}
		return kReturn;
	}


	/**
	 * Opens up the background popup menu.
	 * @param mouseEvent
	 */
	private void popupBackground(MouseEvent mouseEvent) {

		// build VOI intensity popup menu
		JPopupMenu popupMenu = new JPopupMenu();

		JMenuItem menuItem; 
		//menuItem = new JMenuItem("Center root node");
		//popupMenu.add(menuItem);
		//menuItem.addActionListener(this);
		//menuItem.setActionCommand("Center root node" );
		menuItem = new JMenuItem("Set background color");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Set background color" );
		
		menuItem = new JMenuItem("Set background image");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Set background image" );
		
		/*menuItem = new JMenuItem("Increase Font Size");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Increase Font Size" );
		menuItem = new JMenuItem("Decrease Font Size");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Decrease Font Size" );*/

		popupMenu.show(this, mouseEvent.getX(), mouseEvent.getY() );
	}

	/**
	 * Opens the node popup menu.
	 * @param mouseEvent
	 * @param node
	 */
	private void popupEdge(MouseEvent mouseEvent, Edge edge) {
		pickedEdge = edge;

		// build VOI intensity popup menu
		JPopupMenu popupMenu = new JPopupMenu();

		JMenuItem menuItem = new JMenuItem("Delete edge");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Delete edge" );


		popupMenu.show(this, mouseEvent.getX(), mouseEvent.getY() );
	}

	/**
	 * Opens the node popup menu.
	 * @param mouseEvent
	 * @param node
	 */
	private void popupNode(MouseEvent mouseEvent, Node node) {
		pickedNode = node;

		// build VOI intensity popup menu
		JPopupMenu popupMenu = new JPopupMenu();

		JMenuItem menuItem;

		menuItem = new JMenuItem("Add child node");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Add child node" );

		menuItem = new JMenuItem("Delete node");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Delete node" );

		/*menuItem = new JMenuItem("Edit Notes");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Edit Notes" );*/
		
		menuItem = new JMenuItem("Properties");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Properties");
		
		if ( (pickedNodePrev != null) && (pickedNodePrev != pickedNode) )
		{
			menuItem = new JMenuItem("Link nodes");
			popupMenu.add(menuItem);
			menuItem.addActionListener(this);
			menuItem.setActionCommand("Link nodes" );
		}

		/*menuItem = new JMenuItem("Set node color");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Set node color" );

		menuItem = new JMenuItem("Set tree level color");
		popupMenu.add(menuItem);
		menuItem.addActionListener(this);
		menuItem.setActionCommand("Set tree level color" );*/

		popupMenu.show(this, mouseEvent.getX(), mouseEvent.getY() );
	}

	/**
	 * Resets the colors of the picked nodes to the original colors.
	 */
	private void resetPicked()
	{
		if ( pickedNodePrev != null )
		{
			//System.err.println( "ResetPicked PREV" );
			AttributeManager attrMgr = getGraph().getAttributeManager();	
			Color original = (Color)attrMgr.getAttribute( "OriginalColor", pickedNodePrev );	
			if ( original != null )
			{
				attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNodePrev, original );	
			}
			pickedNodePrev = null;
			repaint();
		}
		if ( pickedNode != null )
		{
			//System.err.println( "ResetPicked" );
			AttributeManager attrMgr = getGraph().getAttributeManager();		
			Color original = (Color)attrMgr.getAttribute( "OriginalColor", pickedNode );
			if ( original != null )
			{
				attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, pickedNode, original );
			}
			pickedNode = null;
			repaint();
		}
	}
	
	
	
	public void setLogo(Image l) {
		super.setLogo(l);
		logo = l;
	}
	
	
	
	
	
	
	
	public synchronized void setBGImageShowing(boolean isBGImageShowing) {
		this.isBGImageShowing = isBGImageShowing;
	}


	public synchronized boolean isBGImageShowing() {
		return isBGImageShowing;
	}


	public void paint(Graphics g) {

		if(!isBGImageShowing) {
			super.paint(g);
			return;
		}
		synchronized (kGraph) {
			checkLayout();
			Graphics2D g2 = (Graphics2D) g;
			
			

			if (getUI().isDraft()) {
				g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
									RenderingHints.VALUE_ANTIALIAS_OFF);
				g2.setRenderingHint(RenderingHints.KEY_RENDERING,
									RenderingHints.VALUE_RENDER_SPEED);
				g2.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS,
									RenderingHints.VALUE_FRACTIONALMETRICS_OFF);
			} else {
				g2.setRenderingHint(
					RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);
			}
			//super.paint(g);
			
			
			
			
			
			//Component comp = this.getComponent(0).getComponentAt(0, 1);

			/*int width = getSize().width;
			int height = getSize().height;
			newImage = image.getScaledInstance(width, height, Image.SCALE_DEFAULT);
			
			setLogo(newImage);*/
			

			
		
			if (getWidth() > 300 && getHeight() > 300) {
				if (logo != null)
					g.drawImage(logo, getWidth() - logo.getWidth(this),
									  getHeight() - logo.getHeight(this), this);
			}
			
			
			GraphLayoutModel glm;
			glm = getGraphLayout().getGraphLayoutModel();

			for (Iterator i = getVisibleEdgeIterator(); i.hasNext();) {
				Edge edge = (Edge) i.next();
				if (edge != getHoverElement()) {
					getEdgeRenderer().configure(this, edge);
					paintRenderer(g, getEdgeRenderer());
				}
			}
			for (Iterator i = getVisibleNodeIterator(); i.hasNext();) { 
				Node node = (Node) i.next();
		
				if (node != getHoverElement()) {
					ModelPoint mp = glm.getNodePosition(node);
					getNodeRenderer().configure(this, mp, node);
					paintRenderer(g, getNodeRenderer());
				}
			}
			if (getHoverElement() != null) {
				if (getHoverElement().getElementType() == Element.EDGE_ELEMENT) {
					getEdgeRenderer().configure(this, (Edge) getHoverElement());
					paintRenderer(g, getEdgeRenderer());
				}
				if (getHoverElement().getElementType() == Element.NODE_ELEMENT) {
					ModelPoint mp = glm.getNodePosition((Node) getHoverElement());
					getNodeRenderer().configure(this, mp, (Node) getHoverElement());
					paintRenderer(g, getNodeRenderer());
				}
			}
		}
		/*if (getWidth() > 300 && getHeight() > 300) {
			if (logo != null)
				g.drawImage(logo, getWidth() - logo.getWidth(this),
								  getHeight() - logo.getHeight(this), this);
		} else {
			if (smallLogo != null)
				g.drawImage(smallLogo, getWidth() - smallLogo.getWidth(this),
									   getHeight() - smallLogo.getHeight(this), this);
		}*/
	}
	
	 /**
     * Launches browser...code obtained from: Bare Bones Browser Launch by Dem Pilafian Web Page Copyright (c) 2007
     * Center Key Software Source Code and Javadoc are Public Domain http://www.centerkey.com/java/browser
     * 
     * @param url
     */
    public void openURL(String url) {

        String osName = System.getProperty("os.name");
        try {
            if (osName.startsWith("Mac OS")) {
                Class fileMgr = Class.forName("com.apple.eio.FileManager");
                Method openURL = fileMgr.getDeclaredMethod("openURL", new Class[] {String.class});
                openURL.invoke(null, new Object[] {url});
            } else if (osName.startsWith("Windows")) {
                Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + url);
            } else { // assume Unix or Linux
                String[] browsers = {"firefox", "opera", "konqueror", "epiphany", "mozilla", "netscape"};
                String browser = null;
                for (int count = 0; count < browsers.length && browser == null; count++) {
                    if (Runtime.getRuntime().exec(new String[] {"which", browsers[count]}).waitFor() == 0) {
                        browser = browsers[count];
                    }
                }
                if (browser == null) {
                    System.out.println("Can not find web browser");
                } else {
                    Runtime.getRuntime().exec(new String[] {browser, url});
                }
            }
        } catch (Exception e) {
            System.out.println("Can not find web browser");
        }
    }
	
	
	
	
	
	
	/**
     * launches the dialog to add info
     * 
     * @author pandyan
     * 
     */
    public class PropertiesDialog extends JDialog implements ChangeListener{
    	
    	private JDialogHyperGraph owner;
    	
    	private MipavGraphPanel mgp;
    	
    	private  JTabbedPane tabbedPane = new JTabbedPane();

        private JPanel mainPanel;

        private GridBagConstraints gbc;
        
        private boolean addNode;


        
        public PropertiesDialog(JDialogHyperGraph owner, MipavGraphPanel mgp, boolean addNode) {
        	super(owner,false);
        	this.owner = owner;
        	this.mgp = mgp;
        	this.addNode = addNode;
        	m_kLastCommand = "Edit Notes";
        	init();
        }
        
        public void init() {
        	if(addNode) {
        		setTitle("Add Child Node");
        	}else {
        		setTitle("Properties for " + pickedNode.getLabel() + " node");
        	}
        	
        	AttributeManager attrMgr = getGraph().getAttributeManager();	
			String kNotes = (String)attrMgr.getAttribute( "ANNOTATION", pickedNode );	
			Color currentColor = (Color)attrMgr.getAttribute(GraphPanel.NODE_FOREGROUND, pickedNode);
			addNodeDialog = new JDialogAddNode(mgp, pickedNode, kNotes, addNode, false);
			
			String action = (String)attrMgr.getAttribute( "ACTION", pickedNode );
			actionDialog = new JDialogAction(mgp,pickedNode,action, false);
			
			nodeColorChooser = new ViewJColorChooser(null, "Pick color", mgp, mgp, false );
			/*JPanel prevPanel = new JPanel();
			treeCheckBox = new JCheckBox("Set nodes at same tree level to chosen color");
			prevPanel.add(treeCheckBox);
			prevPanel.add(new JLabel("TESTING"));
			nodeColorChooser.setPreviewPanel(prevPanel);*/
			
			JPanel prevPanel = new JPanel();
			treeCheckBox = new JCheckBox("Set nodes at same tree level to chosen color");
			prevPanel.add(treeCheckBox);
			prevPanel.setSize(prevPanel.getPreferredSize()); 
			prevPanel.setBorder(BorderFactory.createEmptyBorder(0,0,1,0));
			
			//prevPanel.setBorder(WidgetFactory.buildTitledBorder("Testing"));

			nodeColorChooser.setPreviewPanel(prevPanel);

			
			
					
			nodeColorChooser.setColor(currentColor);
			JDialog nodeColorDialog = nodeColorChooser.getColorDialog();

			//((JRootPane)nodeColorDialog.getComponent(0)).getComponent(1).setVisible(false);
			
			treeLevelColorChooser = new ViewJColorChooser(null, "Pick color", mgp, mgp, false );
			JDialog treeLevelColorDialog = treeLevelColorChooser.getColorDialog();
			
			tabbedPane.addChangeListener(this);
        	tabbedPane.addTab("Edit Name/Notes", addNodeDialog.getRootPane());
        	tabbedPane.addTab("Node Color", nodeColorDialog.getRootPane());
        	tabbedPane.addTab("Action", actionDialog.getRootPane());
        	//tabbedPane.addTab("Tree Level Color", treeLevelColorDialog.getRootPane());
        	
             mainPanel = new JPanel(new GridBagLayout());
             
             
             gbc = new GridBagConstraints();
        	 gbc.gridx = 0;
             gbc.gridy = 0;
             gbc.insets = new Insets(10, 5, 10, 25);
             gbc.gridwidth = 1;
             
             mainPanel.add(tabbedPane, gbc);
             
             getContentPane().add(mainPanel);

             pack();
             MipavUtil.centerInWindow(owner, this);
             this.setMinimumSize(this.getSize());
             setVisible(true);
             
             
        }

		@Override
		public void stateChanged(ChangeEvent e) {
			int selectedIndex = tabbedPane.getSelectedIndex();
			if(selectedIndex == 0) {
				m_kLastCommand = "Edit Notes";
			}else if(selectedIndex == 1) {
				m_kLastCommand = "Set node color";
			}else if(selectedIndex == 2) {
				m_kLastCommand = "Set tree level color";
			}
			
		}
    	
    	
    	
    }
	
	
	
	
	
	
	
	
	
	
	
	
	
}
