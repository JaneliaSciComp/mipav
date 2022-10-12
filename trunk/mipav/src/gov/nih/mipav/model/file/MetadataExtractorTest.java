package gov.nih.mipav.model.file;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.io.Serializable;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CharsetDecoder;


import java.lang.reflect.Array;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import gov.nih.mipav.model.file.MetadataExtractorTest.CanonMakernoteDescriptorTest;

import java.util.Iterator;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.Collection;

import java.math.RoundingMode;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/*
 * Ported to MIPAV by William Gandler
 * Copyright 2002-2019 Drew Noakes and contributors
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * More information about this project is available at:
 *
 *    https://drewnoakes.com/code/exif/
 *    https://github.com/drewnoakes/metadata-extractor
 */


public class MetadataExtractorTest extends MetadataExtractor {
	
	public MetadataExtractorTest() {
		
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class CanonMakernoteDescriptorTest
	{
		public CanonMakernoteDescriptorTest() {
			
		}
	    //@Test
		//MetadataExtractorTest me = new MetadataExtractorTest();
    	//CanonMakernoteDescriptorTest cm = me.new CanonMakernoteDescriptorTest();
    	//try {
    	//    cm.testGetFlashBiasDescription();
    	//}
    	//catch (Exception e) {
    	//	e.printStackTrace();
    	//}
		//Finished running testGetFlashBiasDescription()
	    public void testGetFlashBiasDescription() throws Exception
	    {
	        CanonMakernoteDirectory directory = new CanonMakernoteDirectory();
	        CanonMakernoteDescriptor descriptor = new CanonMakernoteDescriptor(directory);

	        // set and check values

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0xFFC0);
	        assertEquals("-2.0 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0xffd4);
	        assertEquals("-1.375 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0000);
	        assertEquals("0.0 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x000c);
	        assertEquals("0.375 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0010);
	        assertEquals("0.5 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0014);
	        assertEquals("0.625 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0020);
	        assertEquals("1.0 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0030);
	        assertEquals("1.5 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0034);
	        assertEquals("1.625 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0040);
	        assertEquals("2.0 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));
	        System.out.println("Finished running testGetFlashBiasDescription()");
	    }
	}
	
	/**
	 * JUnit test case for class ExifReader.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class ExifReaderTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
    	//ExifReaderTest er = me.new ExifReaderTest();
		public ExifReaderTest() {
			
		}
	    @NotNull
	    public Metadata processBytes(@NotNull String filePath) throws IOException
	    {
	        Metadata metadata = new Metadata();
	        byte[] bytes = FileUtil.readBytes(filePath);
	        new ExifReader().extract(new ByteArrayReader(bytes), metadata, ExifReader.JPEG_SEGMENT_PREAMBLE.length(), null);
	        return metadata;
	    }

	    @NotNull
	    public <T extends Directory> T processBytes(@NotNull String filePath, @NotNull Class<T> directoryClass) throws IOException
	    {
	        T directory = processBytes(filePath).getFirstDirectoryOfType(directoryClass);
	        assertNotNull(directory);
	        return directory;
	    }

	    //@SuppressWarnings("ConstantConditions")
	    //@Test
	    //try {
	    //	er.testExtractWithNullDataThrows();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testExtractWithNullDataThrows()
	    public void testExtractWithNullDataThrows() throws Exception
	    {
	        try{
	            new ExifReader().readJpegSegments(null, new Metadata(), JpegSegmentType.APP1);
	            fail("Exception expected");
	        } catch (NullPointerException npe) {
	            // passed
	        }
	        System.out.println("Finished running testExtractWithNullDataThrows()");
	    }

	    //@Test
	  //try {
	    //	er.testLoadFujifilmJpeg();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testLoadFujifilmJpeg()
	    public void testLoadFujifilmJpeg() throws Exception
	    {
	        ExifSubIFDDirectory directory = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/withExif.jpg.app1", ExifSubIFDDirectory.class);

	        final String description = directory.getDescription(ExifSubIFDDirectory.TAG_ISO_EQUIVALENT);
	        assertNotNull(description);
	        assertEquals("80", description);
	        System.out.println("Finished running testLoadFujifilmJpeg()");
	        // TODO decide if this should still be returned -- it was being calculated upon setting of a related tag
//	      assertEquals("F9", directory.getDescription(ExifSubIFDDirectory.TAG_APERTURE));
	    }

	    //@Test
	    public void testReadJpegSegmentWithNoExifData() throws Exception
	    {
	        byte[] badExifData = new byte[]{ 1,2,3,4,5,6,7,8,9,10 };
	        Metadata metadata = new Metadata();
	        ArrayList<byte[]> segments = new ArrayList<byte[]>();
	        segments.add(badExifData);
	        new ExifReader().readJpegSegments(segments, metadata, JpegSegmentType.APP1);
	        assertEquals(0, metadata.getDirectoryCount());
	        assertFalse(metadata.hasErrors());
	    }

	    //@Test
	    public void testCrashRegressionTest() throws Exception
	    {
	        // This image was created via a resize in ACDSee.
	        // It seems to have a reference to an IFD starting outside the data segment.
	        // I've noticed that ACDSee reports a Comment for this image, yet ExifReader doesn't report one.
	        ExifSubIFDDirectory directory = processBytes("Tests/Data/crash01.jpg.app1", ExifSubIFDDirectory.class);

	        assertTrue(directory.getTagCount() > 0);
	    }

	    //@Test
	    public void testDateTime() throws Exception
	    {
	        ExifIFD0Directory directory = processBytes("Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifIFD0Directory.class);

	        assertEquals("2002:11:27 18:00:35", directory.getString(ExifIFD0Directory.TAG_DATETIME));
	    }

	    //@Test
	    public void testThumbnailXResolution() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        Rational rational = directory.getRational(ExifThumbnailDirectory.TAG_X_RESOLUTION);
	        assertNotNull(rational);
	        assertEquals(72, rational.getNumerator());
	        assertEquals(1, rational.getDenominator());
	    }

	    //@Test
	    public void testThumbnailYResolution() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        Rational rational = directory.getRational(ExifThumbnailDirectory.TAG_Y_RESOLUTION);
	        assertNotNull(rational);
	        assertEquals(72, rational.getNumerator());
	        assertEquals(1, rational.getDenominator());
	    }

	    //@Test
	    public void testThumbnailOffset() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        assertEquals(192, directory.getInt(ExifThumbnailDirectory.TAG_THUMBNAIL_OFFSET));
	    }

	    //@Test
	    public void testThumbnailLength() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        assertEquals(2970, directory.getInt(ExifThumbnailDirectory.TAG_THUMBNAIL_LENGTH));
	    }

	    //@Test
	    public void testCompression() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        // 6 means JPEG compression
	        assertEquals(6, directory.getInt(ExifThumbnailDirectory.TAG_COMPRESSION));
	    }

	    //@Test
	    public void testStackOverflowOnRevisitationOfSameDirectory() throws Exception
	    {
	        // An error has been discovered in Exif data segments where a directory is referenced
	        // repeatedly.  Thanks to Alistair Dickie for providing the sample data used in this
	        // unit test.

	        Metadata metadata = processBytes("Tests/Data/recursiveDirectories.jpg.app1");

	        // Mostly we're just happy at this point that we didn't get stuck in an infinite loop.

	        assertEquals(5, metadata.getDirectoryCount());
	    }

	    //@Test
	    public void testDifferenceImageAndThumbnailOrientations() throws Exception
	    {
	        // This metadata contains different orientations for the thumbnail and the main image.
	        // These values used to be merged into a single directory, causing errors.
	        // This unit test demonstrates correct behaviour.
	        Metadata metadata = processBytes("Tests/Data/repeatedOrientationTagWithDifferentValues.jpg.app1");
	        ExifIFD0Directory ifd0Directory = metadata.getFirstDirectoryOfType(ExifIFD0Directory.class);
	        ExifThumbnailDirectory thumbnailDirectory = metadata.getFirstDirectoryOfType(ExifThumbnailDirectory.class);

	        assertNotNull(ifd0Directory);
	        assertNotNull(thumbnailDirectory);

	        assertEquals(1, ifd0Directory.getInt(ExifIFD0Directory.TAG_ORIENTATION));
	        assertEquals(8, thumbnailDirectory.getInt(ExifThumbnailDirectory.TAG_ORIENTATION));
	    }

	/*
	    public void testUncompressedYCbCrThumbnail() throws Exception
	    {
	        String fileName = "withUncompressedYCbCrThumbnail.jpg";
	        String thumbnailFileName = "withUncompressedYCbCrThumbnail.bmp";
	        Metadata metadata = new ExifReader(new File(fileName)).extract();
	        ExifSubIFDDirectory directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);

	        fileName = "withUncompressedYCbCrThumbnail2.jpg";
	        thumbnailFileName = "withUncompressedYCbCrThumbnail2.bmp";
	        metadata = new ExifReader(new File(fileName)).extract();
	        directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);
	        fileName = "withUncompressedYCbCrThumbnail3.jpg";
	        thumbnailFileName = "withUncompressedYCbCrThumbnail3.bmp";
	        metadata = new ExifReader(new File(fileName)).extract();
	        directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);
	        fileName = "withUncompressedYCbCrThumbnail4.jpg";
	        thumbnailFileName = "withUncompressedYCbCrThumbnail4.bmp";
	        metadata = new ExifReader(new File(fileName)).extract();
	        directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);
	    }

	    public void testUncompressedRGBThumbnail() throws Exception
	    {
	        String fileName = "withUncompressedRGBThumbnail.jpg";
	        String thumbnailFileName = "withUncompressedRGBThumbnail.bmp";
	        Metadata metadata = new ExifReader(new File(fileName)).extract();
	        ExifSubIFDDirectory directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);
	    }
	*/
	}

	
	/**
	 * Unit tests for {@link ExifIFD0Descriptor}.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class ExifIFD0DescriptorTest
	{
		public ExifIFD0DescriptorTest() {
			
		}
	    //@Test
	    public void testXResolutionDescription() throws Exception
	    {
	        ExifIFD0Directory directory = new ExifIFD0Directory();
	        directory.setRational(ExifIFD0Directory.TAG_X_RESOLUTION, new Rational(72, 1));
	        // 2 is for 'Inch'
	        directory.setInt(ExifIFD0Directory.TAG_RESOLUTION_UNIT, 2);
	        ExifIFD0Descriptor descriptor = new ExifIFD0Descriptor(directory);
	        assertEquals("72 dots per inch", descriptor.getDescription(ExifIFD0Directory.TAG_X_RESOLUTION));
	        System.out.println("Finished running testXResolutionDescription()");
	    }

	    //@Test
	    public void testYResolutionDescription() throws Exception
	    {
	        ExifIFD0Directory directory = new ExifIFD0Directory();
	        directory.setRational(ExifIFD0Directory.TAG_Y_RESOLUTION, new Rational(50, 1));
	        // 3 is for 'cm'
	        directory.setInt(ExifIFD0Directory.TAG_RESOLUTION_UNIT, 3);
	        ExifIFD0Descriptor descriptor = new ExifIFD0Descriptor(directory);
	        assertEquals("50 dots per cm", descriptor.getDescription(ExifIFD0Directory.TAG_Y_RESOLUTION));
	        System.out.println("Finished running testYResolutionDescription()");
	    }

	    //@Test
	    public void testWindowsXpFields() throws Exception
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        ExifIFD0Directory directory = er.processBytes("Tests/Data/windowsXpFields.jpg.app1", ExifIFD0Directory.class);

	        assertEquals("Testing artist\0", directory.getString(ExifIFD0Directory.TAG_WIN_AUTHOR, "UTF-16LE"));
	        assertEquals("Testing comments\0", directory.getString(ExifIFD0Directory.TAG_WIN_COMMENT, "UTF-16LE"));
	        assertEquals("Testing keywords\0", directory.getString(ExifIFD0Directory.TAG_WIN_KEYWORDS, "UTF-16LE"));
	        assertEquals("Testing subject\0", directory.getString(ExifIFD0Directory.TAG_WIN_SUBJECT, "UTF-16LE"));
	        assertEquals("Testing title\0", directory.getString(ExifIFD0Directory.TAG_WIN_TITLE, "UTF-16LE"));

	        ExifIFD0Descriptor descriptor = new ExifIFD0Descriptor(directory);
	        assertEquals("Testing artist", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_AUTHOR));
	        assertEquals("Testing comments", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_COMMENT));
	        assertEquals("Testing keywords", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_KEYWORDS));
	        assertEquals("Testing subject", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_SUBJECT));
	        assertEquals("Testing title", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_TITLE));
	        System.out.println("Finished running testWindowsXpFields()");
	    }
	}


}