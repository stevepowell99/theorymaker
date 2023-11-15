SUPER_ADMIN <- "STEVE-P"
SUPER_USER <- "steve@pogol.net"
examples<-list.files("assets/examples")
prompts<-list.files("assets/prompts")

map1 <- 'digraph TheoryOfChange {
    rankdir=BT; // This will make the graph bottom-up with the main outcome at the top
    node [shape=box, style="filled,rounded", color=lightblue, fontname="Sans", margin="0.3,0.1"];

    label="my first ToC";
    labelloc=top;
    fontsize=24;
    fontname="Sans";

    // Main outcome formatting
    K [label="Improved educational outcomes\nand life opportunities for children", color=gold, fontsize=20];

    // Nodes
    F [label="Increased motivation for\nstudents to attend school"];
    G [label="Parents prioritize sending\nchildren to school"];
    H [label="Educators use more effective\nteaching methods"];
    I [label="Improved learning environment"];
    J [label="Increased school attendance"];

    // Interventions Subgraph
    subgraph cluster_Interventions {
        label = "Interventions";
        style="filled,rounded";
        color=white;
        fontname="Sans";

        A [label="Provide school supplies\nto students"];
        B [label="Train educators in engaging\nteaching methods"];
        C [label="Community awareness campaigns\non the importance of education"];
        D [label="Improve school infrastructure\n(e.g., classrooms, toilets)"];
        E [label="Establish school\nfeeding programs"];
    }

    // Edges
    A -> F [label="Students have necessary materials"];
    B -> H [label="Educators are better equipped"];
    C -> G [label="Parents see value in education"];
    D -> I [label="Safe and conducive learning environment"];
    E -> F [label="Students receive meals at school"];
    F -> J;
    G -> J;
    H -> J;
    I -> J;
    J -> K [label="Consistent attendance\nleads to better learning"];
}
<- <- '


panzoomjs  <- 'var element = document.getElementById("ss_map");
var panzoom = Panzoom(element, {
maxScale: 5
});
var z = 1;
$("#zoomout").on("click", function(){
z *= 0.9;
panzoom.zoom(z, { animate: true });
});
$("#zoomin").on("click", function(){
z *= 1.1;
panzoom.zoom(z, { animate: true });
});
$("#reset").on("click", function(){
z = 1;
panzoom.reset();
});
'
copyDivJS <- "shinyjs.copyRichText = function( id ) {

var contents = document.getElementById( id ).innerHTML.trim();

const listener = function(ev) {
ev.preventDefault();
ev.clipboardData.setData('text/html', contents);
ev.clipboardData.setData('text/plain', contents);
};
document.addEventListener('copy', listener);
document.execCommand('copy');
document.removeEventListener('copy', listener);
}
"
