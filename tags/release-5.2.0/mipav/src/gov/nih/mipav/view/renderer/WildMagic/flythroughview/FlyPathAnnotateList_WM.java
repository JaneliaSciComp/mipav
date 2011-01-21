package gov.nih.mipav.view.renderer.WildMagic.flythroughview;

import java.util.ArrayList;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Container which stores annotations which include the following: - point position and normal - camera location, view up and view direction, branch and location
 * along branch when point annotated - geometry and appearance - description.
 */
public class FlyPathAnnotateList_WM {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Single annotation item. Must extend Object in order to be placed into a container.
     */
    public class Item extends Object {

        /** Annotation flag indicating whether moving was forward */
        private boolean m_bPathMoveForward;
        /** Annotation relative distance (in the [0,1] range) along the branch path */
        private float m_fNormalizedPathDist;
        /** Annotation branch index */
        private int m_iBranch;
        /** Annotation description */
        private String m_kDescription;
        /** Normal vector for the annotated point */
        private Vector3f m_kPointNormal;
        /** 3D coordinates for the annotated point */
        private Vector3f m_kPointPosition;
        /** Camera location at Annotation point. */
        private Vector3f m_kCameraLocation;
        /** Camera direction vector at Annotation point. */
        private Vector3f m_kCameraDVector;
        /** Camera up vector at Annotation point. */
        private Vector3f m_kCameraUVector;
        /** Camera right vector at Annotation point. */
        private Vector3f m_kCameraRVector;


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
         * @param  kDescription         String Text description to set.
         * @param kCLoc                 Camera location.
         * @param kCDir                 Camera view direction.
         * @param kCUp                  Camera up vector.
         * @param kCRight               Camera right vector.
         */
        Item(int iBranch, float fNormalizedPathDist, boolean bPathMoveForward, Vector3f kPointPosition,
             Vector3f kPointNormal, Vector3f kCLoc, Vector3f kCDir, Vector3f kCUp, Vector3f kCRight, String kDescription) {
            m_iBranch = iBranch;
            m_fNormalizedPathDist = fNormalizedPathDist;
            m_bPathMoveForward = bPathMoveForward;
            m_kPointPosition = new Vector3f(kPointPosition);
            m_kPointNormal = new Vector3f(kPointNormal);
            m_kCameraLocation = new Vector3f(kCLoc);
            m_kCameraDVector = new Vector3f(kCDir);
            m_kCameraUVector = new Vector3f(kCUp);
            m_kCameraRVector = new Vector3f(kCRight);
            m_kDescription = new String(kDescription);
        }

        /**
         * Return the index of the branch from which the item was seen and annotated.
         * @return  int Index which identifies the branch in the FlyPathGraph.
         */
        public int getBranchIndex() {
            return m_iBranch;
        }

        /**
         * Return the Camera direction vector at the Annotation point.
         * @return Camera direction vector at the Annotation point
         */
        public Vector3f getCameraDirection()
        {
            return m_kCameraDVector;
        }

        /**
         * Return the Camera 3D position at the Annotation point.
         * @return Camera 3D position at the Annotation point
         */
        public Vector3f getCameraLocation()
        {
            return m_kCameraLocation;
        }

        /**
         * Return the Camera right vector at the Annotation point.
         * @return Camera right vector at the Annotation point
         */
        public Vector3f getCameraRight()
        {
            return m_kCameraRVector;
        }
        /**
         * Return the Camera up vector at the Annotation point.
         * @return Camera up vector at the Annotation point
         */
        
        public Vector3f getCameraUp()
        {
            return m_kCameraUVector;
        }
        
        /**
         * Return the description associated with the annotated point.
         * @return  String Text description.
         */
        public String getDescription() {
            return m_kDescription;
        }
        
        /**
         * Return the relative distance (in the [0,1] range) along the branch path from which the item was seen and
         * annotated.
         * @return  float
         */
        public float getNormalizedPathDist() {
            return m_fNormalizedPathDist;
        }
        
        /**
         * Return the normal vector for the annotated point.
         * @param  kPointNormal  Vector3f Filled in with the normal vector upon return.
         */
        public void getPointNormal(Vector3f kPointNormal) {
            kPointNormal.Copy(m_kPointNormal);
        }
        
        /**
         * Return the 3D coordinates for the annotated point.
         * @param  kPointPosition  Point3f Filled in with the 3D coordinates upon return.
         */
        public void getPointPosition(Vector3f kPointPosition) {
            kPointPosition.Copy(m_kPointPosition);
        }

        /**
         * Return the flag indicating whether moving was forward, as opposed to in reverse, down the path.
         * @return  boolean True if moving forward down the path.
         */
        public boolean isPathMoveForward() {
            return m_bPathMoveForward;
        }

        /**
         * Set the description associated with the annotated point.
         * @param  kDescription  String Text description to set.
         */
        public void setDescription(String kDescription) {
            m_kDescription = new String(kDescription);
        }
    }

    /** Annotation item number. */
    protected int m_iNextItemKey = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /** Annotation list */
    protected ArrayList<Item> m_kList = new ArrayList<Item>();

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Constructor. Initially an empty list.
     */
    public FlyPathAnnotateList_WM() {
        // See member data declarations for initializations.
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
     * @param kCLoc                 Camera location.
     * @param kCDir                 Camera view direction.
     * @param kCUp                  Camera up vector.
     * @param kCRight               Camera right vector.
     */
    public void addItem(int iBranch, float fNormalizedPathDist, boolean bPathMoveForward, Vector3f kPointPosition,
                        Vector3f kPointNormal, Vector3f kCLoc, Vector3f kCDir, Vector3f kCUp, Vector3f kCRight) {
        String kDescription = new String("Point" + Integer.toString(m_iNextItemKey + 1));

        addItem(new Item(iBranch, fNormalizedPathDist, bPathMoveForward, kPointPosition, kPointNormal,
                kCLoc, kCDir, kCUp, kCRight,
                         kDescription));
    }

    /**
     * Add the specified annotation item to the end of the list.
     * @param  kNewItem  Item Information about the annotation item to be added.
     */
    public void addItem(Item kNewItem) {

        if (null != kNewItem) {
            m_kList.add(kNewItem);
            ++m_iNextItemKey;
        }
    }

    /**
     * Return the specified annotation item.
     * @param   iItem  Index of the specified item.
     * @return  Item Information about the annotation item.
     */
    public Item getItem(int iItem) {
        return m_kList.get(iItem);
    }

    /**
     * Return the number of annotation items stored.
     * @return  int Number of annotation items.
     */
    public int getNumItems() {
        return m_kList.size();
    }

    /**
     * Remove the specified annotation item from the list.
     * @param  iItem  Index of the annotation item in the list to be removed.
     */
    public void removeItem(int iItem) {
        m_kList.remove(iItem);
    }
}
