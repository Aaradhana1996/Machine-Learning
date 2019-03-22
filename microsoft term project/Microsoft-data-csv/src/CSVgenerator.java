import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

import javax.swing.plaf.synth.SynthSeparatorUI;


public class CSVgenerator {
	
	private static ArrayList<String> vroots = new ArrayList<String>();

    public static void main(String[] args) throws FileNotFoundException {
    	PrintWriter pw = new PrintWriter(new File("msweb-test.csv"));
        StringBuilder sb = new StringBuilder();

        System.out.println("done!");
        Scanner scanner = new Scanner(new File("data/anonymous-msweb-test.csv"));
        scanner.useDelimiter(",|\r\n");
        int index = -1;
        String vStr = "";
        boolean addNumbers=false;
        while(scanner.hasNext()){
        	String next = scanner.next();
        	if(next.equals("C")) {
        		index += 1;
        		if(!vStr.isEmpty()) {
	        		vStr = vStr.substring(0, vStr.length() - 1);
	        		sb.append(vStr);
	                sb.append('\n');
	        		vroots.add(vStr);
	        		System.out.println(vStr);
	        		vStr = "";
        		}
        		addNumbers = false;
        		//System.out.println("hereherehere");
        	}
        	else if(next.equals("V")) {
        		addNumbers = true;
        		//System.out.println("V");
        	}
        	else if(next.equals(1)) {
        		//do nothing
        	}
        	else if(!next.equals("1") && addNumbers){
        		vStr += next+",";
        		//System.out.println("here");
        	}
        	else {
        		//System.out.println(next+"|");
        		//.out.println("weird behavour");
        	}
        }
        sb.append(vStr);
        sb.append('\n');
		vroots.add(vStr);
        scanner.close();
        pw.write(sb.toString());
        pw.close();
        //System.out.println(Arrays.toString(vroots.toArray()));
    }

}
