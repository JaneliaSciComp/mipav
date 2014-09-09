package gov.nih.mipav.view.renderer.WildMagic.AAM;
import java.util.*;

public class Test {
	public static void main(String[] args) {
		runTest();
	}
	
	
	public static void runTest() {
		//  *********** block 1 ************
		// Vector<Integer> intArray = new Vector<Integer>();
		// intArray.add(1);
		// intArray.add(2);
		// intArray.add(3);
		/*
		setParam(intArray);
		for ( int i = 0; i < intArray.size(); i++ ) {
			System.err.println(i + " = " + intArray.elementAt(i));
		}
		*/
	
		
		/*
		Point3 pt = new Point3();
		pt.x = 1;
		setValue(pt);
		System.err.println("pt.x = " +  pt.x);
		*/
		/*
		Vector<Item> array = new Vector<Item>();
		
		for ( int i = 0; i < 5; i++ ) {
			Item entry = new Item(); 
			entry.x = i;
			array.add(entry);
		}
		
		for ( int i = 0; i < 5; i++ ) {
			System.err.println(array.get(i).x);
		}
		*/
		String test = new String();
		test = "test.txt";
		int index = test.indexOf(".");
		
		System.err.println(test.substring(0, index));
		
	}
	
	public static void setValue(Point3 p) {
		/*
		Point3 temp = new Point3();
		temp.x = 5;
		
	    p.assign(temp);
		*/
		p.x = 5;
	}
	
	public static void setParam(Vector<Integer> intArray) {
		
		//  *** block 1  **********
		// intArray = new Vector<Integer>();	
		intArray.add(0, 4);
		intArray.add(1, 5);
		intArray.add(2, 6);
		
		/*
		Vector<Integer> copyArray = intArray;	
		copyArray.add(1);
		copyArray.add(2);
		copyArray.add(3);
		*/
	}
	
	
	
}


class Item {
	int x;
	
	public void run() {
		
	}
}
