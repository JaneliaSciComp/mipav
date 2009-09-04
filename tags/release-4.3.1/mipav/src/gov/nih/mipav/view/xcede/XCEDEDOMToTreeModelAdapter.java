package gov.nih.mipav.view.xcede;

import javax.swing.event.EventListenerList;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import java.util.EventListener;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;

public class XCEDEDOMToTreeModelAdapter implements TreeModel {
    public static final String[] typeName =
    {
        "none", "Element", "Attr", "Text", "CDATA", "EntityRef", "Entity",
        "ProcInstr", "Comment", "Document", "DocType", "DocFragment",
        "Notation",
    };
    /**
     * The list of displayable elements.
     */
    private static String[] displayableElementNames =
    {
        "projectlevel",
        "subjectlevel",
        "visitlevel",
        "studylevel",
        "serieslevel",
        "project",
        "subject",
        "visit",
        "study",
        "series",
        "datarec",
        "datarecFrag",
        "ID",
        "description",
        "funding",
        "provenance",
        "statistics",
        "annotation",
        "birthDate",
        "deathDate",
        "name",
        "sex",
        "species",
        "commonName",
        "latinName",
        "strain",
        "extendedDescriptor",
        "value",
        "actualValue",
        "dataClassification",
        "units",
        "text",
        "annotator",
        "timeStamp",
        "subjectVar",
        "assessment",
        "assessmentValue",
        "summaryValue",
        "scanner",
        "manufacturer",
        "model",
        "additionalEquipment",
        "expProtocol",
        "events",
        "event",
        "onset",
        "duration",
        "acqProtocol",
        "acqParam",
        "rasorigin",
        "dimension",
        "origin",
        "size",
        "spacing",
        "gap",
        "direction",
        "byteorder",
        "elementtype",
        "filename",
        "fileoffset",
        "filerecordsize",
        "provanance",
        "processStep",
        "programName",
        "programArgument",
        "version",
        "cvs",
        "user",
        "machine",
        "platform",
        "platformVersion",
        "compilerName",
        "compilerVersion",
        "libName",
        "libVersion",
        "statistic",
        "sourceData",
        "process",
        "activationParams",
        "thresh",
        "pUncorrected",
        "pCorrected",
        "expVoxPerCluster",
        "expNumClusters",
        "df",
        "fwhmSmoothness",
        "fwhm",
        "searchVol",
        "S",
        "voxelSize",
        "vox",
        "clusters",
        "numClusters",
        "p",
        "extents",
        "label",
        "laterization",
        "X",
        "Y",
        "Z",
        "size",
        "voxel",
        "location"
    };

    private Document document;
    
    protected EventListenerList listenerList = new EventListenerList();
    
    public XCEDEDOMToTreeModelAdapter(Document document){
        this.document = document;
    }
    
    /**
     * Checks whether the element with the elementName name is displayable or not.
     * @param elementName  the element name.
     * @return             true if this element is displayable.
     */
    public static boolean isDisplayable(String elementName){
        for(int i = 0;i < displayableElementNames.length; i++){
            if(elementName.equals(displayableElementNames[i])){
                return true;
            }
        }
        return false;
    }
    
    /**
     * 
     */
    public Object getRoot() {
        if(document == null){
            return null;
        }
        NodeList nodeList = document.getChildNodes();
        for(int i = 0; i < nodeList.getLength(); i++){
            Node node = nodeList.item(i);
            if(node.getNodeType() == Node.ELEMENT_NODE){
                return new XCEDEAdapterNode(node);
            }
        }
        return null;
    }

    /**
     * 
     */
    public Object getChild(Object parent, int index) {
        XCEDEAdapterNode adapterNode = (XCEDEAdapterNode)parent;
        return adapterNode.child(index);
    }

    public int getChildCount(Object parent) {
        XCEDEAdapterNode adapterNode = (XCEDEAdapterNode)parent;
        return adapterNode.countChildren();
    }

    public boolean isLeaf(Object node) {
        XCEDEAdapterNode adapterNode = (XCEDEAdapterNode)node;
        if(adapterNode.countChildren() > 0){
            return false;
        }
        return true;
    }

    public int getIndexOfChild(Object parent, Object child) {
        XCEDEAdapterNode adapterNode = (XCEDEAdapterNode)parent;
        return adapterNode.index((XCEDEAdapterNode)child);
    }


    public void valueForPathChanged(TreePath path, Object newValue) {
        // Null. We won't be making changes in the GUI
        // If we did, we would ensure the new value was really new,
        // adjust the model, and then fire a TreeNodesChanged event.
    }

    public void addTreeModelListener(TreeModelListener l) {
        listenerList.add(TreeModelListener.class, l);
    }

    public void removeTreeModelListener(TreeModelListener l) {
        listenerList.remove(TreeModelListener.class, l);
    }

    public void fireTreeNodesChanged(TreeModelEvent e) {
        EventListener[] listeners = listenerList.getListeners(TreeModelListener.class);
        for(int i = 0; i < listeners.length; i++) {
            ((TreeModelListener) listeners[i]).treeNodesChanged(e);
        }
    }

    public void fireTreeNodesInserted(TreeModelEvent e) {
        EventListener[] listeners = listenerList.getListeners(TreeModelListener.class);
        for(int i = 0; i < listeners.length; i++) {
            ((TreeModelListener) listeners[i]).treeNodesInserted(e);
        }
    }

    public void fireTreeNodesRemoved(TreeModelEvent e) {
        EventListener[] listeners = listenerList.getListeners(TreeModelListener.class);
        for(int i = 0; i < listeners.length; i++) {
            ((TreeModelListener) listeners[i]).treeNodesRemoved(e);
        }
    }

    public void fireTreeStructureChanged(TreeModelEvent e) {
        EventListener[] listeners = listenerList.getListeners(TreeModelListener.class);
        for(int i = 0; i < listeners.length; i++) {
            ((TreeModelListener) listeners[i]).treeStructureChanged(e);
        }
    }

}
