import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.view.*;

import info.clearthought.layout.*;

import java.awt.Dimension;
import java.util.List;

import javax.swing.*;


/**
 * Container class to hold tabs for each data structure
 * 
 * @author jhunter
 */
public class PlugInGenericDataEntryContainer extends JDialogStandalonePlugin {
    private static final long serialVersionUID = 5480334277478582203L;

    private static JTabbedPane pane = new JTabbedPane();

    /**
     * @param selections A list of Data Structure short names you wish to add data to
     */
    public PlugInGenericDataEntryContainer(final List<String> selections) {
        final double[][] frameSize = { {TableLayoutConstants.FILL}, {TableLayoutConstants.FILL}};
        setLayout(new TableLayout(frameSize));

        // String targetDataStruct = (String)JOptionPane.showInputDialog(new PlugInDataStructureSelection(), "Please Select A
        // Data Structure to Edit"
        // , "Choose A Data Structure", JOptionPane.QUESTION_MESSAGE, null, null, null);
        if (selections.isEmpty()) {
            PlugInGenericDataEntryContainer.pane.addTab("", new PlugInDataInputTable());
        } else {
            for (final String sel : selections) {
                PlugInGenericDataEntryContainer.pane.addTab(sel.toUpperCase(), new PlugInDataInputTable(sel));
            }
        }

        add(PlugInGenericDataEntryContainer.pane, "0,0");

        Icon icon = null;
        try {
            icon = new ImageIcon(MipavUtil.getIconImage(Preferences.getIconName()));
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final Exception e) {

        }
        setTitle("Genomics and Clinical Assessments Data Entry Tool");
        setSize(new Dimension(1070, 500));
        setName("Data Entry Tool");
        setVisible(true);
    }

    public static void addPane(final List<String> name, final List<String> cols, final List<PlugInGenomicsEntry> entries) {
        PlugInGenericDataEntryContainer.pane.addTab(name.get(0).toUpperCase() + name.get(1).toUpperCase()
                + " (From File)", new PlugInDataInputTable(name, cols, entries));
        PlugInGenericDataEntryContainer.pane.setSelectedIndex(PlugInGenericDataEntryContainer.pane.getTabCount() - 1);
    }
}
