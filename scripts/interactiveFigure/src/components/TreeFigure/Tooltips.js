/** @jsx jsx */
import { css, jsx } from '@emotion/core'
import React from "react";
import ReactTooltip from "react-tooltip";



export default function ToolTips(){
    return (
            <ReactTooltip css={css`font-size:10px`} className = {"toolTip"} id="tip-label" type='light' effect={"solid"}   delayHide={200}  place={'right'} delayUpdate={50}/>
    )
}
