import React from "react";
import {Branches} from "figtreejs-react";
export default function BranchesLayer(props){
    const {colorScale,colorKey}  = props;
    return (
        <>
            <Branches.Coalescent filter={(e => e.v0.node.children.length > 2)} attrs={{
                strokeWidth: 1,
                stroke: e => e.v1.node.annotations[colorKey] ? colorScale(e.v1.node.annotations[colorKey]) : "#9F9B96",


            }}/>
            <Branches.Rectangular filter={(e => e.v0.node.children.length <= 2)} attrs={{
                strokeWidth: 1,
                stroke: e => e.v1.node.annotations[colorKey] ? colorScale(e.v1.node.annotations[colorKey]) : "#9F9B96",

            }}/>
        </>)
}