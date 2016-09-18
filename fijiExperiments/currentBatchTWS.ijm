function action(input, output, filename) {
        //open(input + filename);
        //makeRectangle(10, 10, 300, 180);
        //run("Crop");
        call("trainableSegmentation.Weka_Segmentation.applyClassifier",
input, filename, "showResults=true", 
"storeResults=true", "probabilityMaps=false", "/home/dan/Documents/GitArchive/cvCells/macro-tests-output/TWS");
//call("trainableSegmentation.Weka_Segmentation.getResult");
        //saveAs("Jpeg", output + filename);
        //close();
}


// maybe set “showResults=false”


//input = "/Users/danny/Research/cvCells/dproc/images";
//output = "/Users/danny/Research/cvCells/dproc/images";

input = "/home/dan/Documents/GitArchive/cvCells/dproc/8bits/sams";
output = "/home/dan/Documents/GitArchive/cvCells/macro-tests-output/TWS";

setBatchMode(true); 
list = getFileList(input);
for (i = 0; i < list.length; i++)
        action(input, output, list[i]);
setBatchMode(false);
