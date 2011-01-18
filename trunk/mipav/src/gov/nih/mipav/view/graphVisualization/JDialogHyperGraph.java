package gov.nih.mipav.view.graphVisualization;


import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewMenuBar;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import hypergraph.applications.hexplorer.ContentHandlerFactory;
import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Graph;
import hypergraph.graphApi.GraphSystem;
import hypergraph.graphApi.GraphSystemFactory;
import hypergraph.graphApi.Node;
import hypergraph.graphApi.algorithms.GraphUtilities;
import hypergraph.graphApi.io.CSSColourParser;
import hypergraph.graphApi.io.GraphWriter;
import hypergraph.graphApi.io.GraphXMLWriter;
import hypergraph.graphApi.io.SAXReader;
import hypergraph.visualnet.ArrowLineRenderer;
import hypergraph.visualnet.GraphPanel;
import hypergraph.graph.GraphImpl;
import hypergraph.graph.NodeImpl;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.vecmath.Color3f;

import org.xml.sax.SAXException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.Preferences;

public class JDialogHyperGraph extends JDialogBase {

	/** */
	private static final long serialVersionUID = 7133468293112430462L;

	JTextField m_kInputGraphTextField = null;

	/** current directory * */
	private String m_kCurrentDir = null;
	private String m_kFileName = null;
	
	private ModelImage m_kImage = null;
	
	private Color m_kLineColor = null;
	private Color m_kTextColor = null;
	private Color m_kBackgroundColor = null;
	private Color[] fixedColor = new Color[255];

	private float[] m_afTextScale = new float[] {.75f, .5f, .25f, 0};
	private float[] m_afTextSize = new float[] {12, 10, 8, 0};
	
	private JDialogHyperGraph m_kParent = null;

	public JDialogHyperGraph(boolean bRead, JDialogHyperGraph kParent) {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		m_kParent = kParent;
		initInput(bRead);
	}

	public JDialogHyperGraph(ViewJFrameImage kParent, ModelImage kImage) {
		super(kParent, false);
		ModelLUT lut = new ModelLUT(ModelLUT.STRIPED, 256, new int[] {4, 256});		
		for (int n=0;n<255;n++) fixedColor[n] = lut.getColor(n+1);
		initImage(kParent, kImage);   
		buildMenu();   
		setSize( 900, 600 );
		setVisible(true);
	}
	
	public JDialogHyperGraph(String dir, String file) {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		ModelLUT lut = new ModelLUT(ModelLUT.STRIPED, 256, new int[] {4, 256});		
		for (int n=0;n<255;n++) fixedColor[n] = lut.getColor(n+1);
		if ( dir == null || file == null )
		{
			return;
		}
		init(dir, file);      
		buildMenu();
		setSize( 900, 600 );
		setVisible(true);
	}
	/** Stores the graph that the applet shows. */
	private GraphPanel graphPanel;

	public GraphPanel getGraphPanel() {
		return graphPanel;
	}
	/**@inheritDoc */
	public void init( String dir, String file ) {
		m_kCurrentDir = dir;
		m_kFileName = file;
		setTitle("Graph/Network Visualization");
		GraphSystem graphSystem = null;
		try {
			graphSystem = GraphSystemFactory.createGraphSystem("hypergraph.graph.GraphSystemImpl", null);
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(8);
		}
		Graph graph = null;
		URL url = null;
		URL codeBase = null;
		try {
			codeBase = new File(dir).toURI().toURL();
		} catch (MalformedURLException e1) {
			System.err.println( "codeBass null" );
			e1.printStackTrace();
		}
		try {
			url = new URL(codeBase, file);
			SAXReader reader = new SAXReader(graphSystem, url);
			ContentHandlerFactory ch = new ContentHandlerFactory();
			ch.setBaseUrl(codeBase);
			reader.setContentHandlerFactory(ch);
			graph = reader.parse();
		} catch (FileNotFoundException fnfe) {
			JOptionPane.showMessageDialog(null,
					"Could not find file " + url.getFile() + ". \n" +
					"Start applet with default graph", "File not found", JOptionPane.ERROR_MESSAGE);
			System.out.println("Exception : " + fnfe);
			fnfe.printStackTrace(System.out);
		} catch (SAXException saxe) {
			JOptionPane.showMessageDialog(null,
					"Error while parsing file" + url.getFile() + ". \n" +
					"Exception : " + saxe + ". \n" +
					"Start applet with default graph", "Parsing error", JOptionPane.ERROR_MESSAGE);
			System.out.println("Exception : " + saxe);
			saxe.getException().printStackTrace();
			saxe.printStackTrace(System.out);
		} catch (Exception e) {
			JOptionPane.showMessageDialog(null,
					"General error while reading file " + url + ". \n" +
					"Exception : " + e + ". \n" +
					"Start applet with default graph", "General error", JOptionPane.ERROR_MESSAGE);
			System.out.println(url);
			System.out.println("Exception : " + e);
			e.printStackTrace(System.out);
		}

		if (graph == null) {
			graph = GraphUtilities.createTree(graphSystem, 2, 3);
		}

		graphPanel = new MipavGraphPanel(graph, null);
		
		loadPreferences();


		graphPanel.setLineRenderer(new ArrowLineRenderer());

		getContentPane().add(graphPanel);
		
	}

	public void loadPreferences()
	{
		String file = new String("mipavGraphLayout.prop");
		File graphPreferencesFile = new File(Preferences.getPreferencesDir(), file);
		URL codeBase = null;
		URL url = null;
		try {
			codeBase = graphPreferencesFile.toURI().toURL();
		} catch (MalformedURLException e1) {
			System.err.println( "Cannot read file " + graphPreferencesFile );
			savePreferences();
		}
		try {
			url = new URL(codeBase, file);
			graphPanel.loadProperties(url.openStream());
		} catch (FileNotFoundException fnfe) {
			System.err.println( "Cannot read file " + graphPreferencesFile );
			savePreferences();
		} catch (Exception e) {
			System.err.println( "Cannot read file " + graphPreferencesFile );
			savePreferences();
		}
		
	}
        
     public void savePreferences()   
     {
		String file = new String("mipavGraphLayout.prop");
		File graphPreferencesFile = new File(Preferences.getPreferencesDir(), file);
		FileWriter kNewFile;
		try {
			kNewFile = new FileWriter( graphPreferencesFile );
	        Enumeration properties = graphPanel.getPropertyManager().propertyNames();
	        while (properties.hasMoreElements()) {
	            String name = (String)properties.nextElement();
	            String property = graphPanel.getPropertyManager().getProperty(name).toString();
	            if ( (name.indexOf(".class") == -1) && (name.indexOf(";") == -1)  && (property.indexOf(" ") == -1) )
	            {
	            	kNewFile.write( name + "=" + property + "\n" );
	            }
	        }
	        kNewFile.close();
		} catch (IOException e) { }
	}

	public String getGraphXML() {
		try {
			OutputStream os = new ByteArrayOutputStream();
			GraphWriter graphWriter = new GraphXMLWriter(new OutputStreamWriter(os));
			graphWriter.write(getGraphPanel().getGraph());
			return os.toString();
		} catch (IOException ioe) {
			ioe.printStackTrace();
			return ioe.toString();
		}
	}

	private void initInput(boolean bRead) {
		setForeground(Color.black);
		setTitle("Graph/Network Visualization");

		final GridBagConstraints gbc = new GridBagConstraints();

		final JPanel mainPanel = new JPanel(new GridBagLayout());
		JLabel inputGraphLabel;
		if ( bRead )
		{
			inputGraphLabel = new JLabel("Input Graph");
		}
		else
		{
			inputGraphLabel = new JLabel("Output Graph");
		}

		m_kInputGraphTextField = new JTextField(20);
		m_kInputGraphTextField.setEditable(false);
		m_kInputGraphTextField.setBackground(Color.white);

		final JButton browseButton = new JButton("Browse");
		browseButton.addActionListener(this);
		if ( bRead )
		{
			browseButton.setActionCommand("inputDirBrowse");
		}
		else
		{
			browseButton.setActionCommand("outputDirBrowse");
		}

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;

		mainPanel.add(inputGraphLabel, gbc);

		gbc.gridx = 1;
		mainPanel.add(m_kInputGraphTextField, gbc);

		gbc.gridx = 2;
		mainPanel.add(browseButton, gbc);
		final JPanel OKCancelPanel = new JPanel();
		buildOKButton();
		OKButton.setActionCommand("OK");
		OKCancelPanel.add(OKButton, BorderLayout.WEST);
		buildCancelButton();
		cancelButton.setActionCommand("cancel");
		OKCancelPanel.add(cancelButton, BorderLayout.EAST);

		getContentPane().add(mainPanel, BorderLayout.CENTER);
		getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
		pack();
		setMinimumSize(getSize());
		// setResizable(false);
		setVisible(true);

	}
	
	private void initImage(ViewJFrameImage kParent, ModelImage kImage)
	{
		m_kImage = kImage;
		JMenuBar kMenu = kParent.getJMenuBar();
		GraphSystem graphSystem = null;
		try {
			graphSystem = GraphSystemFactory.createGraphSystem("hypergraph.graph.GraphSystemImpl", null);
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(8);
		}
		Graph tree = graphSystem.createGraph();
		Node root = tree.createNode();
		root.setLabel( kImage.getImageName() );
		for ( int i = 0; i < kMenu.getMenuCount(); i++ )
		{
			Node menu = tree.createNode();
			menu.setLabel( kMenu.getMenu(i).getText() );
    		AttributeManager attrMgr = tree.getAttributeManager();
    		attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, menu, fixedColor[0] ); 
			tree.createEdge(root, menu );
			addSubTree( tree, menu, kMenu.getMenu(i).getMenuComponents(), 1 );
		}

		graphPanel = new MipavGraphPanel(tree, kParent);

		loadPreferences();

		graphPanel.setLineRenderer(new ArrowLineRenderer());

		getContentPane().add(graphPanel);
	}
	

    public void addSubTree( Graph tree, Node menu, Component[] menuComponents, int level )
    {
        if ( menuComponents == null )
        {
            return;
        }
        for ( int i = 0; i < menuComponents.length; i++ )
        {
            if ( menuComponents[i] instanceof JMenu )
            {
            	Node subMenu = tree.createNode();
            	subMenu.setLabel( ((JMenuItem)menuComponents[i]).getText() );
        		AttributeManager attrMgr = tree.getAttributeManager();
        		attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, subMenu, fixedColor[level] ); 
            	tree.createEdge( menu, subMenu );
    			addSubTree( tree, subMenu, ((JMenu)menuComponents[i]).getMenuComponents(), level+1 );
            }
            else if ( menuComponents[i] instanceof JMenuItem )
            {
            	Node subMenu = tree.createNode();
            	subMenu.setLabel( ((JMenuItem)menuComponents[i]).getActionCommand() );;
        		AttributeManager attrMgr = tree.getAttributeManager();
        		attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, subMenu, fixedColor[level] ); 
            	tree.createEdge( menu, subMenu );
            }
        }
    }

	@Override
	public void actionPerformed(ActionEvent e) {
		final String command = e.getActionCommand();

		if (command.equalsIgnoreCase("inputDirBrowse") || command.equalsIgnoreCase("outputDirBrowse") ) {
			final JFileChooser chooser = new JFileChooser();

			if (m_kCurrentDir != null) {
				chooser.setCurrentDirectory(new File(m_kCurrentDir, m_kFileName));
			}
			chooser.setDialogTitle("Choose image");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", } ) );
			final int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				m_kCurrentDir = chooser.getSelectedFile().getAbsolutePath();
				final FileIO fileIO = new FileIO();
				fileIO.setQuiet(true);
				m_kFileName = chooser.getSelectedFile().getName();
				m_kInputGraphTextField.setText(m_kCurrentDir);
			}
		}
		else if (command.equals("OK")) {
			setVisible(false);
			if ( m_kParent != null )
			{
				m_kParent.writeGraphXML( m_kCurrentDir );
			}
			else
			{
				new JDialogHyperGraph( m_kCurrentDir, m_kFileName);
			}
		}
		else if (command.equals("cancel")) {
			dispose();
		}
		else if (command.equals("SetProperties")) {
			new JDialogGraphProperties(this);
		}
		else if (command.equals("SaveGraphXML")) {
			new JDialogHyperGraph(false, this);
		}
	}
	
	public void setBackgroundColor( Color kColor )
	{
		if ( kColor == null )
		{
			return;
		}
		m_kBackgroundColor = new Color(kColor.getRGB());
		//"hypergraph.hyperbolic.text.fontName"
		//"hypergraph.hyperbolic.text.size1"
		//"hypergraph.hyperbolic.text.scale1"
		//"hypergraph.hyperbolic.text.size2"
		//"hypergraph.hyperbolic.text.scale2"
		//"hypergraph.hyperbolic.text.size3"
		//"hypergraph.hyperbolic.text.scale3"
		//"hypergraph.hyperbolic.text.size4"
		//"hypergraph.hyperbolic.text.scale4"
		//"hypergraph.hyperbolic.text.color"
		//"hypergraph.hyperbolic.line.color"
		String kColorString = new String( "#" + Integer.toHexString(kColor.getRGB()).substring(2) );
		graphPanel.getPropertyManager().setProperty( "hypergraph.hyperbolic.background.color",
				kColorString );
		graphPanel.refreshProperties();
	}
	
	public void setTextColor( Color kColor )
	{
		if ( kColor == null )
		{
			return;
		}
		m_kTextColor = new Color(kColor.getRGB());
		String kColorString = new String( "#" + Integer.toHexString(kColor.getRGB()).substring(2) );
		graphPanel.getPropertyManager().setProperty( "hypergraph.hyperbolic.text.color",
				kColorString );
		graphPanel.refreshProperties();
	}
	
	public void setLineColor( Color kColor )
	{
		if ( kColor == null )
		{
			return;
		}
		m_kLineColor = new Color(kColor.getRGB());
		String kColorString = new String( "#" + Integer.toHexString(kColor.getRGB()).substring(2) );
		graphPanel.getPropertyManager().setProperty( "hypergraph.hyperbolic.line.color",
				kColorString );
		graphPanel.refreshProperties();
	}
	
	public Color getBackgroundColor()
	{
		if ( m_kBackgroundColor == null )
		{
			m_kBackgroundColor = getColorFromString( "hypergraph.hyperbolic.background.color" );
		}
		if ( m_kBackgroundColor == null )
		{
			m_kBackgroundColor = Color.gray;
		}
		return m_kBackgroundColor;
	}
	
	
	public Color getTextColor()
	{
		if ( m_kTextColor == null )
		{
			m_kTextColor = getColorFromString( "hypergraph.hyperbolic.text.color" );
		}
		if ( m_kTextColor == null )
		{
			m_kTextColor = Color.gray;
		}
		return m_kTextColor;
	}
	
	
	public Color getLineColor()
	{
		if ( m_kLineColor == null )
		{
			m_kLineColor = getColorFromString( "hypergraph.hyperbolic.line.color" );
		}
		if ( m_kLineColor == null )
		{
			m_kLineColor = Color.gray;
		}
		return m_kLineColor;
	}
	
	public float[] getTextSize()
	{
		for ( int i = 0; i < 4; i++ )
		{
			String kScaleString = (String)graphPanel.getPropertyManager().getProperty( "hypergraph.hyperbolic.text.size" + (i+1) );
			if ( kScaleString != null )
			{
				m_afTextSize[i] = Float.parseFloat(kScaleString);
			}
		}
		return m_afTextSize;
	}
	
	public void setTextSize( float[] afSize )
	{
		if ( afSize == null )
		{
			return;
		}
		for ( int i = 0; i < 4; i++ )
		{
			m_afTextSize[i] = afSize[i];
			graphPanel.getPropertyManager().setProperty( "hypergraph.hyperbolic.text.size" + (i+1), String.valueOf(m_afTextSize[i]) );
		}
		((MipavGraphPanel)graphPanel).refreshText();
		graphPanel.refreshProperties();
	}
	
	public void increaseTextSize( boolean bBigger )
	{
		for ( int i = 0; i < 4; i++ )
		{
			m_afTextSize[i] *= bBigger ? 1.1 : .9;
			graphPanel.getPropertyManager().setProperty( "hypergraph.hyperbolic.text.size" + (i+1), String.valueOf(m_afTextSize[i]) );
		}
		((MipavGraphPanel)graphPanel).refreshText();
		graphPanel.refreshProperties();
		graphPanel.repaint();
	}
	
	private Color getColorFromString( String key )
	{
		String kColorString = (String)graphPanel.getPropertyManager().getProperty( key );
		return CSSColourParser.stringToColor(kColorString);
	}
	
    /**
     * Builds menus for the User Interface.
     */
    public void buildMenu() {
    	JMenuItem saveGraph = new JMenuItem("Save Graph as XML");
    	saveGraph.addActionListener(this);
    	saveGraph.setActionCommand("SaveGraphXML");
    	JMenuItem properties = new JMenuItem("Properties");
    	properties.addActionListener(this);
    	properties.setActionCommand("SetProperties");
    	JMenu propertiesMenu = new JMenu("File");
    	propertiesMenu.add(saveGraph);
    	propertiesMenu.add(properties);
    	JMenuBar menuBar = new JMenuBar();
    	menuBar.add( propertiesMenu );
    	setJMenuBar(menuBar);
    }
    
    public void writeGraphXML(String file) {
    	File outPut = new File( file );
    	GraphXMLWriter writer;
		try {
			writer = new GraphXMLWriter(outPut);
	    	writer.write(graphPanel.getGraph());
		} catch (IOException e) {
			e.printStackTrace();
		}
    }
}
