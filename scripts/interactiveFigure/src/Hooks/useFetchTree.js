import {useEffect} from "react";
import {
    orderByNodeDensity,
    parseNexus, getTips, annotateNode, getNodes, getNode
} from "figtreejs-react";
const processTree=tree=> {
    return tree;
    // return orderByNodeDensity(tree, false);
};
export default function useFetchTree(path,callback){
    useEffect(()=>{
        ///data/2020-03-10/2020-03-19_nCoV.mcc.tre
        fetch(process.env.PUBLIC_URL+path)
            .then(res=> res.text())
            .then(text=> {
                let tree= processTree(parseNexus(text,{datePrefix: "|",dateFormat:"%Y-%m-%d"})[0]);
                const externalNodes =getTips(tree);//.map(t=>t.name);
                for(const tip of externalNodes) {
                    const Guangdong = tip.name.split("|").reverse()[2]==="Guangdong"?"Guangdong":'Other';
                    tree=annotateNode(tree,tip.id,{"Guangdong":Guangdong})
                }
                callback(tree);
                // console.log(countryContinentMap);
                // setOriginalTree(tree);
                // setTree(tree);
            })
    },[]);

}