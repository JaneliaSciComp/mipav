package gov.nih.mipav.view.renderer.surfaceview.rfaview;

import javax.vecmath.*;
import java.util.*;

/**
 * Information about a set of burns attempting to treat one of the target surfaces from the target list.
 */
public class TreatmentInformation {

    /**
     * List of burn attributes.
     */
    private Vector burns = new Vector();

    /**
     * The total volume of the burns in this treatment set (if it has been calculated and the number of burns hasn't changed).
     */
    private float totalVolume = -1;

    /**
     * The volume difference btw the tumor surface and the buring sphere packings.
     */
    private float diffVolume = -1;

    /**
     * Create a new treatment set information object.
     */
    public TreatmentInformation() {}

    /**
     * Set the total volume of all the burns.
     * @param vol  the total volume
     */
    public void setTotalVolume( float vol ) {
        totalVolume = vol;
    }

    /**
     * Return the total volume of all this treatment set's burns.
     * @return  the total volume
     */
    public float getTotalVolume() {
        return totalVolume;
    }

    /**
     * Return the diff volume of the current treatment.
     * @return  the total volume
     */
    public float getDiffVolume() {
        return diffVolume;
    }

    /**
     * Set the diff volume of the current treatment.
     * @param vol  the total volume
     */
    public void setDiffVolume( float vol ) {
        diffVolume = vol;
    }

    /**
     * Add a burn to the treatment list.
     * @param burn  a new burn
     */
    public void addBurn( BurnAttributes burn ) {
        burns.add( burn );

        // total volume (maybe) calculated previously is now invalid
        setTotalVolume( -1 );
    }

    /**
     * Remove a burn from the treatment list.
     * @param index  the index of the burn to remove
     */
    public void removeBurn( int index ) {
        burns.remove( index );

        // total volume (maybe) calculated previously is now invalid
        setTotalVolume( -1 );
    }

    /**
     * Remove a burn from the treatment list.
     * @param obj  the burn object to remove
     */
    public void removeBurn( Object obj ) {
        burns.remove( obj );

        // total volume (maybe) calculated previously is now invalid
        setTotalVolume( -1 );
    }

    /**
     * Remove all the burns in the treatment list.
     */
    public void removeAllBurns() {
        burns.removeAllElements();

        // total volume (maybe) calculated previously is now invalid
        setTotalVolume( -1 );
    }

    /**
     * Return the number of burns in this treatment's burn list.
     * @return  the number of burns
     */
    public int getNumBurns() {
        return burns.size();
    }

    /**
     * Return one of the treatment's burns.
     * @param index  index into the burn list
     * @return       the requested burn's attributes
     */
    public BurnAttributes getBurn( int index ) {
        return (BurnAttributes) burns.get( index );
    }

    /**
     * Return the list of burns as an enumeration.
     * @return  an enumeration of the treatment's burns
     */
    public Enumeration getBurnEnum() {
        return burns.elements();
    }

    /**
     * Return the center point of a burn.
     * @param index  the index of the burn to get the center of
     * @return  the coordinates of the center of the requested burn
     */
    public Point3f getBurnCenter( int index ) {
        return ( (BurnAttributes) burns.get( index ) ).center;
    }

    /**
     * Return the volume of a burn.
     * @param index  the index of the burn to get the volume of
     * @return  the volume of the requested burn
     */
    public float getBurnVolume( int index ) {
        return ( (BurnAttributes) burns.get( index ) ).volume;
    }

    /**
     * Return the diameter of a burn.
     * @param index  the index of the burn to get the diameter of
     * @return  the diameter of the requested burn
     */
    public Point3f getBurnRadius( int index ) {
        return ( (BurnAttributes) burns.get( index ) ).radius;
    }

    /**
     * Return the name of a burn.
     * @param index  the index of the burn to get the name of
     * @return  the name of the requested burn
     */
    public String getBurnName( int index ) {
        return ( (BurnAttributes) burns.get( index ) ).name;
    }
}
