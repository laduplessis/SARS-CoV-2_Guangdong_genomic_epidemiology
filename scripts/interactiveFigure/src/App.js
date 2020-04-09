/** @jsx jsx */
import { css, jsx } from '@emotion/core'
import React, { useState} from 'react';

import {schemeTableau10,schemeSet3,schemePaired} from "d3-scale-chromatic";
import {scaleOrdinal} from "d3-scale";
import Container from "./components/Container";
import TreeFigure from "./components/TreeFigure";
import Slider from "./components/Slider";


function App() {

    const [isTreeLoaded, setIsTreeLoaded] = useState(false);
    const width = 1100, height = 500, margins = {top: 10, right: 210, bottom: 40, left: 40};

    const [colorKey, setColorKey] = useState("lineage");


    return (
        <div>
            <div css={css`text-align:center;margin:auto;display:${isTreeLoaded ? "none" : "inline"}`}>
                <p>Loading data</p>
            </div>

            <Container>
                <div css={css`flex-basis:95%;`}>
                    <Slider colorKey = {colorKey} setColorKey={setColorKey} display={isTreeLoaded}/>
                    <TreeFigure display={isTreeLoaded}
                                setIsTreeLoaded={setIsTreeLoaded} width={width} height={height} margins={margins} colorKey={colorKey}/>
                </div>
            </Container>
        </div>
    )

}

export default App;

