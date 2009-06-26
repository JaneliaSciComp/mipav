package gov.nih.mipav.view.xcede;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import gov.nih.mipav.view.ViewJFrameImage;

public class XCEDEAdapterNode {
    private Node domNode;
    private ViewJFrameImage openedImageFrame; 
    public XCEDEAdapterNode(Node node){
        this.domNode = node;
    }
    
    public Node getDOMNode(){
        return domNode;
    }
    
    public String getNodeName(){
        return domNode.getNodeName();
    }
    
    public String getNodeValue(){
        return domNode.getNodeValue();
    }
    
    public ViewJFrameImage getOpenedImageFrame(){
        return openedImageFrame;
    }
    
    public void setOpenedImageFrame(ViewJFrameImage imageFrame){
        this.openedImageFrame = imageFrame;
    }
    
    public XCEDEAdapterNode child(String nodeName){
        for(int i = 0; i < domNode.getChildNodes().getLength(); i++){
            Node childNode = domNode.getChildNodes().item(i);
            if(childNode.getNodeName().equals(nodeName)){
                return new XCEDEAdapterNode(childNode);
            }
        }
        return null;
    }
    /**
     * Returns the index of the specific child.
     * @param child  a child node.
     * @return       the index of the specific child.
     */
    public int index(XCEDEAdapterNode child){
        if(child == null){
            return -1;
        }
        int count = countChildren();
        for(int i = 0; i < count; i++){
            XCEDEAdapterNode adapterNode = this.child(i);
            if(child.getDOMNode() == adapterNode.getDOMNode()){
                return i;
            }
        }
        return -1;
    }
    
    public XCEDEAdapterNode child(int index){
        int nodeIndex = 0;
        NodeList nodeList = domNode.getChildNodes();
        Node node  = nodeList.item(index);
        for(int i = 0; i < nodeList.getLength(); i++){
            node = nodeList.item(i);
            if ((node.getNodeType() == Node.ELEMENT_NODE) &&
                 XCEDEDOMToTreeModelAdapter.isDisplayable(node.getNodeName()) &&
                 (nodeIndex++ == index)) {
                break;
            }
        }
        return new XCEDEAdapterNode(node);
    }
    
    public int countChildren(){
        int count = 0;
        NodeList nodeList = domNode.getChildNodes();
        for(int i = 0; i < nodeList.getLength(); i++){
            Node node = nodeList.item(i);
            if((node.getNodeType() == Node.ELEMENT_NODE) &&
                XCEDEDOMToTreeModelAdapter.isDisplayable(node.getNodeName())){
                ++count;
            }
        }
        return count;
    }
    
    public String content(){
        String s = "";
        org.w3c.dom.NodeList nodeList = domNode.getChildNodes();

        for (int i = 0; i < nodeList.getLength(); i++) {
            org.w3c.dom.Node node = nodeList.item(i);
            int type = node.getNodeType();
            XCEDEAdapterNode adpNode = new XCEDEAdapterNode(node); //inefficient, but works

            if (type == Node.ELEMENT_NODE) {
                // Skip subelements that are displayed in the tree.   
                if (XCEDEDOMToTreeModelAdapter.isDisplayable(node.getNodeName())) {
                    continue;
                }

                // EXTRA-CREDIT HOMEWORK:
                //   Special case the SLIDE element to use the TITLE text
                //   and ignore TITLE element when constructing the tree.
                s += ("<" + node.getNodeName() + ">");
                s += adpNode.content();
                s += ("</" + node.getNodeName() + ">");
            } else if (type == Node.TEXT_NODE) {
                s += node.getNodeValue();
            } else if (type == Node.ENTITY_NODE) {
                // The content is in the TEXT node under it
                s += adpNode.content();
            } else if (type == Node.CDATA_SECTION_NODE) {
                // The "value" has the text, same as a text node.
                //   while EntityRef has it in a text node underneath.
                //   (because EntityRef can contain multiple subelements)
                // Convert angle brackets and ampersands for display
                StringBuffer sb = new StringBuffer(node.getNodeValue());

                for (int j = 0; j < sb.length(); j++) {
                    if (sb.charAt(j) == '<') {
                        sb.setCharAt(j, '&');
                        sb.insert(j + 1, "lt;");
                        j += 3;
                    } else if (sb.charAt(j) == '&') {
                        sb.setCharAt(j, '&');
                        sb.insert(j + 1, "amp;");
                        j += 4;
                    }
                }

                s += ("<pre>" + sb + "\n</pre>");
            }

            // Ignoring these:
            //   ATTR_TYPE      -- not in the DOM tree
            //   ENTITY_TYPE    -- does not appear in the DOM
            //   PROCINSTR_TYPE -- not "data"
            //   COMMENT_TYPE   -- not "data"
            //   DOCUMENT_TYPE  -- Root node only. No data to display.
            //   DOCTYPE_TYPE   -- Appears under the root only
            //   DOCFRAG_TYPE   -- equiv. to "document" for fragments
            //   NOTATION_TYPE  -- nothing but binary data in here
        }

        return s;
    }
    public String toString(){
        String s = XCEDEDOMToTreeModelAdapter.typeName[domNode.getNodeType()];
        if(!s.equals("Document")){
            s = "";
        }
        String nodeName = domNode.getNodeName();

        if (!nodeName.startsWith("#")) {
            s += nodeName;
        }
        return s;
    }
}
