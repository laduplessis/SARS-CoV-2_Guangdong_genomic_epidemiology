/** @jsx jsx */
import {css, jsx} from '@emotion/core';
import React from "react";

const s = `position: relative;
                display: inline-block;
                width: 60px;
                height: 34px;
                    input {opacity: 0;
                        width: 0;
                        height: 0;
                    }`;

const slider = ` position: absolute;
  cursor: pointer;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: #ccc;
  -webkit-transition: .4s;
  transition: .4s;

   :before {
  position: absolute;
  content: "";
  height: 26px;
  width: 26px;
  left: 4px;
  bottom: 4px;
  background-color: white;
  -webkit-transition: .4s;
  transition: .4s;
}
`

const input = `:checked + span {
  background-color: #ccc;
}

:focus + span {
  box-shadow: 0 0 1px #2196F3;
}

:checked + span:before {
  -webkit-transform: translateX(26px);
  -ms-transform: translateX(26px);
  transform: translateX(26px);
}`;
const textLabels = `padding-left:4px;padding-right:4px`;
const container = `display:flex;flex-direction:row; justify-content:center;align-items:center`;
export default function Slider(props) {
    const {colorKey, setColorKey, display} = props;
    if (display) {
        return (
            <div css={css`${container}`}>
                <p css={css`${textLabels}`}>Lineage</p>
                <label css={css`${s}`}>
                    <input css={css`${input}`} type="checkbox" onClick={() => setColorKey(getNewColorKey(colorKey))}/>
                    <span css={css`${slider}`}></span>
                </label>
                <p css={css`${textLabels}`}>Location</p>
            </div>
        )
    } else {
        return null;
    }
}

// onChange={setColorKey(getNewColorKey(colorKey))}
function getNewColorKey(colorKey) {
    return colorKey === "lineage" ? "Guangdong" : "lineage";
}