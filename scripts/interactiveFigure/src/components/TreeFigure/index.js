
import React, {useEffect,useState} from "react"
import {timeFormat} from "d3-time-format";
import ReactTooltip from "react-tooltip";
import useFetchTree from "../../Hooks/useFetchTree";
import {getDateRange,FigTree,Axis,AxisBars,Legend,highlightedVertices,collapseNodes,Nodes} from "figtreejs-react";
import {scaleOrdinal, scaleTime} from "d3-scale";
import BranchesLayer from "./Branches";
import NodesLayers from "./NodesLayer";
import ToolTips from "./Tooltips"
import {schemeTableau10} from "d3-scale-chromatic";
export default function TreeFigure(props) {

    const {width, height,display, margins, setIsTreeLoaded,colorKey} = props;

    const [tree,setTree]=useState(null);
    const [originalTree,setOriginalTree] = useState(null);



    useFetchTree("/data/MCC_tree_annotated.tree",
        (tree, map) => {

        const t = collapseNodes(tree,n=>n.annotations.posterior<0.5);
            setTree(t);
            console.log(t);
            setOriginalTree(t);
            setIsTreeLoaded(true)
        });

    useEffect(()=>{
        ReactTooltip.rebuild();
    },[tree]);


    if(display) {

        const colorScaleDomain = originalTree.annotationTypes[colorKey].values;
        const timeScale = scaleTime().domain(getDateRange(tree)).range([0,(width-margins.left-margins.right)]);


        const scheme = schemeTableau10.slice(0,5)
            .concat("#9F9B96");
        const locationScheme = ["#A79D96","#BF0F34"];
        const colorScale = scaleOrdinal().domain(colorScaleDomain).range(colorKey==="Guangdong"?locationScheme:scheme);

        const activeLocations =tree.annotationTypes[colorKey]? [...tree.annotationTypes[colorKey].values]:[];
        const activeColors = activeLocations.map(c=>colorScale(c));
        const activeColorScale =  scaleOrdinal().domain(activeLocations).range(activeColors);
        return (
            <>
            {/*<input type={"range"} max={1} min={0} step={0.1}/>*/}

                    <FigTree width={width} height={height} margins={margins}
                             tree={tree} pos={{x: margins.left, y: margins.top}} layout={customLayout}>
                        <rect width={width} height={height} fill={"none"}
                              transform={`translate(${-margins.left},${-margins.top})`}
                              pointerEvents={"all"} onClick={() => {
                            setTree(originalTree)
                        }}/>
                        <Axis direction={"horizontal"} scale={timeScale} gap={10}
                              ticks={{number: 10, format: timeFormat("%m-%d"), padding: 20, style: {}, length: 6}}>
                            <AxisBars lift={5}/>
                        </Axis>
                        <Nodes.Coalescent filter={(v => v.node.children && v.node.children.length > 2)}
                                          attrs={{fill: v => (v.node.annotations[colorKey] ? colorScale(v.node.annotations[colorKey]) : "#9F9B96")}}
                                          interactions={{
                                              "onClick": (v) => {
                                                  console.log(v);
                                                  setTree(v.node)
                                              }
                                          }}
                                          startWidth={1}
                                          sortFactor={1}
                        />
                        <BranchesLayer colorScale={colorScale} colorKey={colorKey}/>
                        <NodesLayers colorScale={colorScale} colorKey={colorKey} setTree={setTree} tree={tree}/>

                        <Legend.Discrete height={700} columns={1} width={200} pos={{x: width - 200, y: 0}}
                                         swatchSize={8}
                                         scale={activeColorScale} annotation={colorKey}/>
                    </FigTree>
                <ToolTips toottipContent={""} tree={tree} roottipContent={""} colorKey={colorKey}/>
            </>
        )
    }else{
        return null;
    }
}

function customLayout(tree){
   return highlightedVertices(tree,0.4,n=>n.annotations.lineage)
}