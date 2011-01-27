package gov.nih.mipav.view.graphVisualization;


import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Edge;
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.awt.image.PixelGrabber;
import java.awt.print.PageFormat;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

import org.apache.pdfbox.exceptions.COSVisitorException;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.edit.PDPageContentStream;
import org.apache.pdfbox.pdmodel.font.PDFont;
import org.apache.pdfbox.pdmodel.font.PDType1Font;
import org.apache.pdfbox.pdmodel.graphics.xobject.PDJpeg;
import org.apache.pdfbox.pdmodel.graphics.xobject.PDXObjectImage;
import org.xml.sax.SAXException;

import com.sun.jimi.core.Jimi;
import com.sun.jimi.core.JimiException;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class JDialogHyperGraph extends JFrame implements ActionListener {

	/** */
	private static final long serialVersionUID = 7133468293112430462L;

	JTextField m_kInputGraphTextField = null;

	/** current directory * */
	private String m_kCurrentDir = null;
	private String m_kFileName = null;

	private Color m_kLineColor = null;
	private Color m_kTextColor = null;
	private Color m_kBackgroundColor = null;
	private Color[] fixedColor = new Color[255];

	private float[] m_afTextSize = new float[] {12, 10, 8, 0};

	private PDFont m_kCurrentFont = null;
	private int m_iCurrentFontSize = 0;
	private float m_fPageWidth; 
	private float m_fPageMargin; 
	
	/** Stores the graph that the applet shows. */
	private MipavGraphPanel graphPanel;

	public JDialogHyperGraph(boolean bRead) {
        super();
        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }
		if ( bRead )
		{
			final JFileChooser chooser = new JFileChooser();

			if (m_kCurrentDir != null) {
				chooser.setCurrentDirectory(new File(m_kCurrentDir, m_kFileName));
			}
			chooser.setDialogTitle("Choose Input Graph");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", } ) );
			final int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				m_kCurrentDir = chooser.getSelectedFile().getAbsolutePath();
				final FileIO fileIO = new FileIO();
				fileIO.setQuiet(true);
				m_kFileName = chooser.getSelectedFile().getName();
				new JDialogHyperGraph( m_kCurrentDir, m_kFileName);
			}
		}
	}

	public JDialogHyperGraph(String dir, String file) {
		super();
        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }

		//super(ViewUserInterface.getReference().getMainFrame(), false);
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
	public JDialogHyperGraph(ViewJFrameImage kParent, ModelImage kImage) {
		super();
        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }

		//super(kParent, false);
		ModelLUT lut = new ModelLUT(ModelLUT.STRIPED, 256, new int[] {4, 256});		
		for (int n=0;n<255;n++) fixedColor[n] = lut.getColor(n+1);
		initImage(kParent, kImage);   
		buildMenu();   
		setSize( 900, 600 );
		setVisible(true);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		final String command = e.getActionCommand();
		if ( command.equals("CenterGraph") ) {
			graphPanel.centerRootNode();
		}
		//else if (command.equals("SetProperties")) {
		//	new JDialogGraphProperties(this);
		//}
		else if (command.equals("SaveProperties")) {
			savePreferences();
		}
		else if (command.equals("SaveGraph")) {
			final JFileChooser chooser = new JFileChooser();

			if (m_kCurrentDir != null) {
				chooser.setCurrentDirectory(new File(m_kCurrentDir, m_kFileName));
			}
			chooser.setDialogTitle("Choose Output Graph");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", ".txt", ".pdf", ".jpg", ".tif"} ) );
			final int returnValue = chooser.showSaveDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				File kFile = chooser.getSelectedFile();			
				
				m_kCurrentDir = chooser.getSelectedFile().getAbsolutePath();
				final FileIO fileIO = new FileIO();
				fileIO.setQuiet(true);
				m_kFileName = chooser.getSelectedFile().getName();
				if ( m_kFileName.contains( ".xml" ) )
				{
					writeGraphXML( m_kCurrentDir );
				}
				else if ( m_kFileName.contains( ".txt" ) )
				{
					writeGraphTXT( m_kCurrentDir );
				}
				else if ( m_kFileName.contains( ".pdf" ) )
				{
					writeGraphPDF( chooser.getSelectedFile().getParent(), chooser.getSelectedFile().getName() );
				}
				else if ( m_kFileName.contains( ".jpg" ) )
				{
					writeGraphJPG( chooser.getSelectedFile().getParent(), chooser.getSelectedFile().getName() );
				}
				else if ( m_kFileName.contains( ".tif" ) )
				{
					writeGraphTIF( chooser.getSelectedFile().getParent(), chooser.getSelectedFile().getName() );
				}
				else
				{
					writeGraphXML( m_kCurrentDir );
				}
			}
		}
	}
	public void addSubTree( Graph tree, Node root, Node menu, Component[] menuComponents, int level )
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
				attrMgr.setAttribute( AttributeManager.GRAPH_ROOT, subMenu, root ); 
				attrMgr.setAttribute( "TreeLevel", subMenu, level ); 
				tree.createEdge( menu, subMenu );
				addSubTree( tree, root, subMenu, ((JMenu)menuComponents[i]).getMenuComponents(), level+1 );
			}
			else if ( menuComponents[i] instanceof JMenuItem )
			{
				Node subMenu = tree.createNode();
				subMenu.setLabel( ((JMenuItem)menuComponents[i]).getActionCommand() );;
				AttributeManager attrMgr = tree.getAttributeManager();
				attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, subMenu, fixedColor[level] ); 
				attrMgr.setAttribute( AttributeManager.GRAPH_ROOT, subMenu, root ); 
				attrMgr.setAttribute( "TreeLevel", subMenu, level ); 
				tree.createEdge( menu, subMenu );
			}
		}
	}

	/**
	 * Builds menus for the User Interface.
	 */
	public void buildMenu() {
		JMenuItem centerGraph = new JMenuItem("Center root node");
		centerGraph.addActionListener(this);
		centerGraph.setActionCommand("CenterGraph");
		JMenuItem saveGraph = new JMenuItem("Save Graph as...");
		saveGraph.addActionListener(this);
		saveGraph.setActionCommand("SaveGraph");
		JMenuItem properties = new JMenuItem("Save Graph Properties");
		properties.addActionListener(this);
		properties.setActionCommand("SaveProperties");

		JMenu propertiesMenu = new JMenu("File");
		propertiesMenu.add(centerGraph);
		propertiesMenu.add(saveGraph);
		propertiesMenu.add(properties);
		JMenuBar menuBar = new JMenuBar();
		menuBar.add( propertiesMenu );
		setJMenuBar(menuBar);
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

	public GraphPanel getGraphPanel() {
		return graphPanel;
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

	public void increaseTextSize( boolean bBigger )
	{
		for ( int i = 0; i < 4; i++ )
		{
			m_afTextSize[i] *= bBigger ? 1.1 : .9;
			graphPanel.getPropertyManager().setProperty( "hypergraph.hyperbolic.text.size" + (i+1), String.valueOf(m_afTextSize[i]) );
		}
		graphPanel.refreshText();
		graphPanel.refreshProperties();
		graphPanel.repaint();
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
			MipavSAXReader reader = new MipavSAXReader(graphSystem, url);
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
		graphPanel.refreshText();
		graphPanel.refreshProperties();
	}

	public void writeGraphJPG(String dir, String file)
	{
		Node root = graphPanel.findRoot();
		if ( root == null )
		{			
			root = graphPanel.findMinRoot();
		}
		if ( root == null )
		{
			return;
		}
		writeImage( root.getLabel(), dir, file, null, graphPanel.getWidth(), graphPanel.getHeight() );
	}

	public void writeGraphPDF(String dir, String file)
	{
		HashSet<Node> visitedSet = null;
		Node root = graphPanel.findRoot();
		if ( root == null )
		{			
			visitedSet = new HashSet<Node>();
			root = graphPanel.findMinRoot();
		}
		if ( root == null )
		{
			return;
		}

    	File pdfFile = new File( dir, file );

		// the document
    	PDDocument doc = null;
        PDPageContentStream[] contentStream = new PDPageContentStream[1];
        try
        {
            doc = new PDDocument();

            PDPage page = new PDPage();
            doc.addPage( page );
            m_kCurrentFont = PDType1Font.HELVETICA_BOLD;

            contentStream[0] = new PDPageContentStream(doc, page);
            contentStream[0].beginText();

            contentStream[0].setFont(m_kCurrentFont, 18);
            m_iCurrentFontSize = 18;
            String message  ="MIPAV: Graph Visualization";

            m_fPageWidth = 7f * (PDPage.PAGE_SIZE_LETTER.getWidth() / 8.5f); 
            m_fPageMargin = (PDPage.PAGE_SIZE_LETTER.getWidth() - m_fPageWidth) / 2f;
    		float scale = m_fPageWidth / graphPanel.getWidth();
    		float[] height = new float[]{PDPage.PAGE_SIZE_LETTER.getHeight() - scale * graphPanel.getHeight()};

            height[0] -= 4 * (m_iCurrentFontSize + 4);
            contentStream[0].moveTextPositionByAmount(m_fPageMargin, height[0]);
            contentStream[0].drawString( message );
            
            height[0] -= (m_iCurrentFontSize + 4);
            contentStream[0].moveTextPositionByAmount(0, -(m_iCurrentFontSize + 4));

            contentStream[0].setFont(m_kCurrentFont, 12);
            m_iCurrentFontSize = 12;
            
            writeGraph( visitedSet, doc, contentStream, height, root, 0 );
            
            contentStream[0].endText();
		} catch (Exception e) {
		    e.printStackTrace();

		    if(contentStream[0] != null) {
            	try {
					contentStream[0].close();
				} catch (IOException e1) {
					e.printStackTrace();
					MipavUtil.displayError("Content stream could not be closed, please restart MIPAV.");
				}
            }
			if( doc != null )
            {
                try {
					doc.close();
				} catch (IOException e1) {
					e.printStackTrace();
					MipavUtil.displayError("PDF document could not be closed, please restart MIPAV.");
				}
            }
		} 

		try {
			contentStream[0].close();
			doc.save(pdfFile.toString());
            doc.close();
		} catch(Exception e) {
			e.printStackTrace();
		}
		
		
		doc = null;
		try
		{
			doc = PDDocument.load( dir + File.separator + file );
			PageFormat pf = doc.getPageFormat(0);
			float width = (float)pf.getWidth();
			float height = (float)pf.getHeight();


			String imageName = writeImage( root.getLabel(), dir, file, ".jpg", m_fPageWidth, height );

			//we will add the image to the first page.
			PDPage page = (PDPage)doc.getDocumentCatalog().getAllPages().get( 0 );

			PDXObjectImage ximage = new PDJpeg(doc, new FileInputStream( dir + File.separator + imageName ) );
			contentStream[0] = new PDPageContentStream(doc, page, true, true);
			contentStream[0].drawImage( ximage, m_fPageMargin, height - ximage.getHeight() );

			contentStream[0].close();
			doc.save( pdfFile.toString() );

		} catch (IOException e) {
			e.printStackTrace();
		}
		catch (COSVisitorException e) {
			e.printStackTrace();
		}
		finally
		{
			if( doc != null )
			{
				try {
					doc.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}    	
	}

	public void writeGraphTIF(String dir, String file)
	{
		Node root = graphPanel.findRoot();
		if ( root == null )
		{			
			root = graphPanel.findMinRoot();
		}
		if ( root == null )
		{
			return;
		}
		writeImage( root.getLabel(), dir, file, null, graphPanel.getWidth(), graphPanel.getHeight() );
	}

	public void writeGraphTXT(String file)
	{
		File outPut = new File( file );
		FileWriter kNewFile;
		try {
			kNewFile = new FileWriter( outPut );
			Node root = graphPanel.findRoot();
			if ( root != null )
			{
				writeGraph( null, kNewFile, root, 0 );
			}
			else
			{
				root = graphPanel.findMinRoot();
				HashSet<Node> visitedSet = new HashSet<Node>();
				writeGraph( visitedSet, kNewFile, root, 0 );
			}
			kNewFile.close();
		} catch (IOException e) { }
	}

	public void writeGraphXML(String file) {
		File outPut = new File( file );
		MipavGraphXMLWriter writer;
		try {
			writer = new MipavGraphXMLWriter(outPut);
			writer.write(graphPanel.getGraph());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private Color getColorFromString( String key )
	{
		String kColorString = (String)graphPanel.getPropertyManager().getProperty( key );
		return CSSColourParser.stringToColor(kColorString);
	}

	private void initImage(ViewJFrameImage kParent, ModelImage kImage)
	{
		setTitle("Graph/Network Visualization");
		JMenuBar kMenu = kParent.getJMenuBar();
		GraphSystem graphSystem = null;
		try {
			graphSystem = GraphSystemFactory.createGraphSystem("hypergraph.graph.GraphSystemImpl", null);
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(8);
		}
		Graph tree = graphSystem.createGraph();
		AttributeManager attrMgr = tree.getAttributeManager();
		Node root = tree.createNode();
		root.setLabel( kImage.getImageName() );
		attrMgr.setAttribute( AttributeManager.GRAPH_ROOT, root, root ); 
		attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, root, fixedColor[0] ); 
		attrMgr.setAttribute( "TreeLevel", root, 0 ); 
		for ( int i = 0; i < kMenu.getMenuCount(); i++ )
		{
			Node menu = tree.createNode();
			menu.setLabel( kMenu.getMenu(i).getText() );
			attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, menu, fixedColor[1] ); 
			attrMgr.setAttribute( AttributeManager.GRAPH_ROOT, menu, root ); 
			attrMgr.setAttribute( "TreeLevel", menu, 1 ); 
			tree.createEdge(root, menu );
			addSubTree( tree, root, menu, kMenu.getMenu(i).getMenuComponents(), 2 );
		}

		graphPanel = new MipavGraphPanel(tree, kParent);

		loadPreferences();

		graphPanel.setLineRenderer(new ArrowLineRenderer());

		getContentPane().add(graphPanel);
	}

	private void writeGraph( HashSet<Node> visitedSet, FileWriter kWriter, Node kNode, int iLevel )
	{
		if ( (visitedSet != null) && visitedSet.contains(kNode) )
		{
			return;
		}
		try {
			for ( int i = 0; i < iLevel; i++ )
			{
				kWriter.write( "\t" );
			}
			kWriter.write( kNode.getLabel() + "\n"  );
			if ( visitedSet != null )
			{
				visitedSet.add(kNode);
			}
			Iterator iter = graphPanel.getGraph().getEdges(kNode).iterator();
			while ( iter.hasNext() )
			{
				Edge edge = (Edge) iter.next();
				if ( edge.getSource().equals(kNode) )
				{
					Node target = edge.getTarget();
					writeGraph( visitedSet, kWriter, target, iLevel+1 );
				}
			}		
		} catch (IOException e) {}
	}

	private void writeGraph( HashSet<Node> visitedSet, PDDocument doc, PDPageContentStream[] contentStream, float[] height, Node kNode, int iLevel )
	{
		if ( (visitedSet != null) && visitedSet.contains(kNode) )
		{
			return;
		}
		try {

            contentStream[0].moveTextPositionByAmount(m_fPageMargin * (iLevel+1), 0);
            contentStream[0].drawString( kNode.getLabel() );
            contentStream[0].moveTextPositionByAmount(-(m_fPageMargin  * (iLevel+1)), 0);
            height[0] -= (m_iCurrentFontSize + 4);
            if ( height[0] <= 4 * (m_iCurrentFontSize + 4) )
            {
                contentStream[0].endText();
                contentStream[0].close();

                PDPage page = new PDPage();
                doc.addPage( page );
                PDFont font = PDType1Font.HELVETICA_BOLD;

                contentStream[0] = new PDPageContentStream(doc, page);
                contentStream[0].beginText();

                contentStream[0].setFont(font, 12);
                height[0] = PDPage.PAGE_SIZE_LETTER.getHeight();
                height[0] -= 4 * (m_iCurrentFontSize + 4);
                contentStream[0].moveTextPositionByAmount(0, height[0]);
                height[0] -= (m_iCurrentFontSize + 4);
            }
            contentStream[0].moveTextPositionByAmount(0, -(m_iCurrentFontSize + 4));

			if ( visitedSet != null )
			{
				visitedSet.add(kNode);
			}
			Iterator iter = graphPanel.getGraph().getEdges(kNode).iterator();
			while ( iter.hasNext() )
			{
				Edge edge = (Edge) iter.next();
				if ( edge.getSource().equals(kNode) )
				{
					Node target = edge.getTarget();
					writeGraph( visitedSet, doc, contentStream, height, target, iLevel+1 );
				}
			}		
		} catch (IOException e) {}
	}


	private String writeImage(String node, String dir, String file, String format, float width, float height ) {
		int[] pixels;
		int bufferSize, xDim, yDim;
		short[] buffer = null;
		ModelImage testImage = null;
		String imageName;


		BufferedImage kBufferedImage = null;
		try {
			xDim = graphPanel.getWidth();
			yDim = graphPanel.getHeight();

			kBufferedImage = new BufferedImage( xDim, yDim, BufferedImage.TYPE_INT_ARGB );
			Graphics2D kGraphics = kBufferedImage.createGraphics();
			graphPanel.paint(kGraphics);
			bufferSize = 4 * xDim * yDim;
			pixels = new int[xDim * yDim];

			PixelGrabber pgTest = new PixelGrabber(kBufferedImage, 0, 0, xDim, yDim, pixels, 0, xDim);
			pgTest.grabPixels();

		} catch (InterruptedException e) {
			Preferences.debug("Interrupted waiting for pixels!");

			return null;
		} catch (OutOfMemoryError error) {
			MipavUtil.displayError("ViewFrameImage: unable to allocate enough memory for RGB image");

			return null;
		}


		try {
			int[] extents = new int[2];
			extents[0] = xDim; // RGB
			extents[1] = yDim;

			if ( format == null )
			{
				imageName = file;
			}
			else
			{
				imageName = node + "_graph" + format;
			}
			testImage = new ModelImage(ModelStorageBase.ARGB, extents, node + "_graph" );
			testImage.setImageName(imageName);
			testImage.getFileInfo()[0].setFileDirectory(dir);
			buffer = new short[bufferSize];
		} catch (OutOfMemoryError error) {
			MipavUtil.displayError("JDialogScreenCapture: unable to allocate enough memory for RGB image");

			return null;
		}

		for (int i = 0, k = 0; i < (xDim * yDim); i++, k += 4) {
			buffer[k] = (short) (255); // alpha
			buffer[k + 1] = (short) ((pixels[i] >> 16) & 0xFF); // Red
			buffer[k + 2] = (short) ((pixels[i] >> 8) & 0xFF); // Green
			buffer[k + 3] = (short) (pixels[i] & 0xFF); // Blue
		}

		try {
			testImage.importData(0, buffer, true);
		} catch (IOException error) {
			MipavUtil.displayError("JDialogScreenCapture: Problems grabbing image!");
		}




		float scale = width / graphPanel.getWidth();
		TransMatrix xfrm = new TransMatrix(3);
		xfrm.MakeIdentity();

		Vector3f center = testImage.getImageCentermm(false);
		int[] units = new int[testImage.getUnitsOfMeasure().length];
		for (int i = 0; i < units.length; i++) {
			units[i] = testImage.getUnitsOfMeasure(i);
		}
		testImage.calcMinMax();

		int newXDim = (int)(xDim * scale);
		int newYDim = (int)(yDim * scale);
		float fXRes = xDim * testImage.getResolutions(0)[0] / newXDim;
		float fYRes = yDim * testImage.getResolutions(0)[1] / newYDim;
		AlgorithmTransform transformImage = new AlgorithmTransform(testImage, xfrm, AlgorithmTransform.BSPLINE4, 
				fXRes, fYRes, newXDim, newYDim, units, 
				false, true, false, true, center);
		transformImage.setFillValue((float)testImage.getMin());
		transformImage.setUpdateOriginFlag(true);

		transformImage.run();

		ModelImage resultImage = transformImage.getTransformedImage();
		resultImage.calcMinMax();
		resultImage.setImageName(imageName);

		transformImage.disposeLocal();
		transformImage = null;
		
		if ( imageName.contains( ".jpg" ) )
		{
			int length = newXDim*newYDim;
			kBufferedImage = new BufferedImage( newXDim, newYDim, BufferedImage.TYPE_INT_ARGB );
			int[] imageData = new int[length*4];
			try {
				resultImage.exportData(0, length*4, imageData);
				int[] bufferData = new int[length*4];
				for (int i = 0; i < length; i++)
				{
					bufferData[i*4 + 0] = imageData[i * 4 + 1];
					bufferData[i*4 + 1] = imageData[i * 4 + 2];
					bufferData[i*4 + 2] = imageData[i * 4 + 3];
					bufferData[i*4 + 3] = imageData[i * 4 + 0];
				}
				kBufferedImage.getRaster().setPixels(0,0, newXDim, newYDim, bufferData );
                Jimi.putImage(kBufferedImage, dir + File.separator + imageName);
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (final JimiException e) {
                e.printStackTrace();
            }
		}
		else
		{
			FileIO fileIO = new FileIO();

			FileWriteOptions options = new FileWriteOptions(imageName, dir + File.separator, true);
			fileIO.writeImage(resultImage, options);
		}
		resultImage.disposeLocal();
		testImage.disposeLocal();

		return imageName;
	}


}
