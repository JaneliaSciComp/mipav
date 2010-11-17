
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import info.clearthought.layout.TableLayout;

import java.awt.Dimension;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JTabbedPane;

/**
 * Container class to hold tabs for each data structure 
 * @author jhunter
 * @params a list of Data Structure short names you wish to add data to
 */
public class PlugInGenericDataEntryContainer extends JFrame {
	private static final long serialVersionUID = 5480334277478582203L;
	private static JTabbedPane pane = new JTabbedPane();
	
	public PlugInGenericDataEntryContainer(List<String> selections){
		double[][] frameSize = {{TableLayout.FILL},{TableLayout.FILL}};
		setLayout(new TableLayout(frameSize));
		
//		String targetDataStruct = (String)JOptionPane.showInputDialog(new PlugInDataStructureSelection(), "Please Select A Data Structure to Edit"
//				, "Choose A Data Structure", JOptionPane.QUESTION_MESSAGE, null, null, null);
		if(selections.isEmpty()){
			pane.addTab("",new PlugInDataInputTable());
		}
		else{
			for(String sel : selections)
				pane.addTab(sel.toUpperCase(), new PlugInDataInputTable(sel));
		}
		
		add(pane, "0,0");
		
		Icon icon = null;
        try {
            icon = new ImageIcon(MipavUtil.getIconImage(Preferences.getIconName()));
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final Exception e) {

        }
		setTitle("Genomics and Clinical Assessments Data Entry Tool");
		setSize(new Dimension(1070,500));
		setName("Data Entry Tool");
        setVisible(true);
	}
	
	public static void addPane(List<String> name, List<String> cols, List<PlugInGenomicsEntry> entries){
		pane.addTab(name.get(0).toUpperCase()+name.get(1).toUpperCase()+" (From File)", new PlugInDataInputTable(name,cols,entries));
		pane.setSelectedIndex(pane.getTabCount()-1);
	}
}
