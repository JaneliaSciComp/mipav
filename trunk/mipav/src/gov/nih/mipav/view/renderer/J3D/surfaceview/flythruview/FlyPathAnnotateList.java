package gov.nih.mipav.view.renderer.J3D.surfaceview.flythruview;


import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Container which stores annotations which include the following: - point position and normal - branch and location
 * along branch when point annotated - geometry and appearance - description.
 */
public class FlyPathAnnotateList {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected int m_iNextItemKey = 0;

    /** DOCUMENT ME! */
    protected Shape3D m_kDefaultShape = new Shape3D();

    /** DOCUMENT ME! */
    protected ArrayList<Item> m_kList = new ArrayList<Item>();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor. Initially an empty list.
     */
    public FlyPathAnnotateList() {
        // See member data declarations for initializations.
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Add the specified annotation item to the end of the list.
     *
     * @param  kNewItem  Item Information about the annotation item to be added.
     */
    public void addItem(Item kNewItem) {

        if (null != kNewItem) {
            m_kList.add(kNewItem);
            ++m_iNextItemKey;
        }
    }

    /**
     * Create a new annotation item with the specified information and add the item to the end of the list. Set the
     * item's appearance and geometry based on the defaults. Generate a description for the item based on the number of
     * items inserted already.
     *
     * @param  iBranch              int Index of branch from which the item was seen and annotated.
     * @param  fNormalizedPathDist  float Relative distance along the branch (in the [0,1] range) from which the item
     *                              was seen and annotated.
     * @param  bPathMoveForward     boolean Flag set if the moving forward, as opposed to in reverse, down the path.
     * @param  kPointPosition       Point3f Coordinates of the annotated point.
     * @param  kPointNormal         Vector3f Normal vector associated with the annotated point.
     */
    public void addItem(int iBranch, float fNormalizedPathDist, boolean bPathMoveForward, Point3f kPointPosition,
                        Vector3f kPointNormal) {
        String kDescription = new String("Point" + Integer.toString(m_iNextItemKey + 1));

        addItem(new Item(iBranch, fNormalizedPathDist, bPathMoveForward, kPointPosition, kPointNormal, m_kDefaultShape,
                         kDescription));
    }

    /**
     * Return access to the default shape to use for rendering all subsequent annotated items.
     *
     * @return  Shape3D Instance which contains the geometry and appearance to use for rendering.
     */
    public Shape3D getDefaultShape() {
        return m_kDefaultShape;
    }

    /**
     * Return the specified annotation item.
     *
     * @param   iItem  int Index of the specified item.
     *
     * @return  Item Information about the annotation item.
     */
    public Item getItem(int iItem) {
        return (Item) m_kList.get(iItem);
    }

    /**
     * Return the number of annotation items stored.
     *
     * @return  int Number of annotation items.
     */
    public int getNumItems() {
        return m_kList.size();
    }

    /**
     * Remove the specified annotation item from the list.
     *
     * @param  iItem  int Index of the annotation item in the list to be removed.
     */
    public void removeItem(int iItem) {
        m_kList.remove(iItem);
    }

    /**
     * Set the default shape for rendering all subsequent annotated items to that specified.
     *
     * @param  kShape  Shape3D Instance which contains the geometry and appearance to use for rendering.
     */
    public void setDefaultShape(Shape3D kShape) {
        m_kDefaultShape = (Shape3D) kShape.cloneTree();
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Single annotation item. Must extend Object in order to be placed into a container.
     */
    public class Item extends Object {

        /** DOCUMENT ME! */
        private boolean m_bPathMoveForward;

        /** DOCUMENT ME! */
        private float m_fNormalizedPathDist;

        /** DOCUMENT ME! */
        private int m_iBranch;

        /** DOCUMENT ME! */
        private String m_kDescription;

        /** DOCUMENT ME! */
        private Vector3f m_kPointNormal;

        /** DOCUMENT ME! */
        private Point3f m_kPointPosition;

        /** DOCUMENT ME! */
        private Shape3D m_kShape;

        /**
         * Constructor. Only the shape and description can be changed once initialized in the constructor, while all
         * other member data cannot be changed once initialized.
         *
         * @param  iBranch              int Index of branch from which the item was seen and annotated.
         * @param  fNormalizedPathDist  float Relative distance along the branch (in the [0,1] range) from which the
         *                              item was seen and annotated.
         * @param  bPathMoveForward     boolean Flag set if the moving forward, as opposed to in reverse, down the path.
         * @param  kPointPosition       Point3f Coordinates of the annotated point.
         * @param  kPointNormal         Vector3f Normal vector associated with the annotated point.
         * @param  kShape               Shape3D Instance which contains the geometry and appearance to use for
         *                              rendering.
         * @param  kDescription         String Text description to set.
         */
        Item(int iBranch, float fNormalizedPathDist, boolean bPathMoveForward, Point3f kPointPosition,
             Vector3f kPointNormal, Shape3D kShape, String kDescription) {
            m_iBranch = iBranch;
            m_fNormalizedPathDist = fNormalizedPathDist;
            m_bPathMoveForward = bPathMoveForward;
            m_kPointPosition = new Point3f(kPointPosition);
            m_kPointNormal = new Vector3f(kPointNormal);
            m_kShape = (Shape3D) kShape.cloneTree();
            m_kDescription = new String(kDescription);
        }

        /**
         * Return the index of the branch from which the item was seen and annotated.
         *
         * @return  int Index which identifies the branch in the FlyPathGraph.
         */
        public int getBranchIndex() {
            return m_iBranch;
        }

        /**
         * Return the description associated with the annotated point.
         *
         * @return  String Text description.
         */
        public String getDescription() {
            return m_kDescription;
        }

        /**
         * Return the relative distance (in the [0,1] range) along the branch path from which the item was seen and
         * annotated.
         *
         * @return  float
         */
        public float getNormalizedPathDist() {
            return m_fNormalizedPathDist;
        }

        /**
         * Return the normal vector for the annotated point.
         *
         * @param  kPointNormal  Vector3f Filled in with the normal vector upon return.
         */
        public void getPointNormal(Vector3f kPointNormal) {
            kPointNormal.set(m_kPointNormal);
        }

        /**
         * Return the 3D coordinates for the annotated point.
         *
         * @param  kPointPosition  Point3f Filled in with the 3D coordinates upon return.
         */
        public void getPointPosition(Point3f kPointPosition) {
            kPointPosition.set(m_kPointPosition);
        }

        /**
         * Return access to the shape for rendering the annotated point.
         *
         * @return  Shape3D Instance which contains the geometry and appearance to use for rendering.
         */
        public Shape3D getShape() {
            return m_kShape;
        }

        /**
         * Return the flag indicating whether moving was forward, as opposed to in reverse, down the path.
         *
         * @return  boolean True if moving forward down the path.
         */
        public boolean isPathMoveForward() {
            return m_bPathMoveForward;
        }

        /**
         * Set the description associated with the annotated point.
         *
         * @param  kDescription  String Text description to set.
         */
        public void setDescription(String kDescription) {
            m_kDescription = new String(kDescription);
        }

        /**
         * Set the shape for rendering the annotated point to that specified.
         *
         * @param  kShape  Shape3D Instance which contains the geometry and appearance to use for rendering.
         */
        public void setShape(Shape3D kShape) {
            m_kShape = kShape;
        }
    }
}
