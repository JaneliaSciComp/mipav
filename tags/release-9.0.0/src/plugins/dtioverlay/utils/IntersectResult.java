package dtioverlay.utils;

/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Nov 20, 2005
 * Time: 11:12:10 AM
 * To change this template use Options | File Templates.
 * ************************************
 * Magnetic Resonance in Medicine Final Project
 * Released: December 1, 2005
 *
 * class IntersectResult
 *      Store the results associated with detecting an intersection.
 *
 * Copyright (C) 2005 Bennett Landman, bennett@bme.jhu.edu
 */
public class IntersectResult {
    public PT intersectionPoint;   // store the point of intersection
    public float fractionalDistance; // store the fractional distance between the ends of a line segement until intersection
    public PT intersectionNormal; //normal vector at point of intercept
    public char resultCode;
    // Create a new IntersectionResult
    public IntersectResult(PT a, float s, PT norm) {
        intersectionPoint = a;
        fractionalDistance = s;
        intersectionNormal = norm;
    }

    public IntersectResult(char code, PT a) {
        intersectionPoint = a;
        resultCode = code;
    }
}
