/** @jsx jsx */
import {css, jsx} from "@emotion/core";
import React from "react"
import {InteractionContainer} from "figtreejs-react";

const container = `display: flex; 
                   flex-direction: row;
                    padding-left: 2%; 
                    padding-right:2%; 
                    padding-top: 5%; 
                    flex-wrap: nowrap; 
                    justify-content: space-around;
                    align-items: center;`;
export default function Container(props){
    return(
        <div css={css`${container}`}>
        <InteractionContainer>
            {props.children}
        </InteractionContainer>
    </div>)
}