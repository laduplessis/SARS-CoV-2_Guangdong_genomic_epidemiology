import React from "react";
import {Nodes,NodeBackgrounds,getTips, getNode, useFigtreeContext} from "figtreejs-react";
import {max, min} from "d3-array";

export default function NodesLayers(props){
    const {colorScale,colorKey,setTree,tree,} = props;
    const {height} = useFigtreeContext.scales();
    const nodeWidth = min([10,max([2,height/2/getTips(tree).length])]);
    return(
        <>
            <NodeBackgrounds.Circle filter={(v => !v.node.children)}
                                    attrs={{r: nodeWidth,fill:"white", strokeWidth: 1, stroke:  v => colorScale(v.node.annotations[colorKey]),}}
                                    hoveredAttrs={{r: nodeWidth + 4 }}/>
            <Nodes.Rectangle
                filter={(v => v.node.children && v.node.children.length > 2)}
                attrs={{
                    cursor: "pointer",
                    fill: "none",
                    pointerEvents: "all",
                    width: 50,
                    height: 20,
                    transform: `translate(0,-${20 / 2})`
                }}
                interactions={{
                    "onClick": (v) => {
                        setTree(v.node)
                    }
                }}
                hoverKey={null}/>
            <Nodes.Circle tooltip={{'data-tip': v => `${v.id.split("|").slice(1).join("|")} [set:${getNode(tree,v.id).annotations[colorKey]}]`, "data-for": "tip-label"}}
                          filter={(v => !v.node.children)}
                          attrs={{
                              r: nodeWidth,
                              fill: v => colorScale(v.node.annotations[colorKey]),
                              strokeWidth: 0,
                              opacity:0.7}}
                          hoveredAttrs={{r: nodeWidth + 4}}/>
            </>
    )
}