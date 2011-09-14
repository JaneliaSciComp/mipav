package dtioverlay.utils;

/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Nov 20, 2005
 * Time: 10:13:50 AM
 * To change this template use Options | File Templates.
 * ************************************
 * Magnetic Resonance in Medicine Final Project
 * Released: December 1, 2005
 *
 * class DegenerateIntersectionException
 *      This exception indicates that the intersection is degenerate.
 *
 * Copyright (C) 2005 Bennett Landman, bennett@bme.jhu.edu
 */
public class DegenerateIntersectionException extends Exception {
    public DegenerateIntersectionException(String s) {
        super(s);
    }
}
