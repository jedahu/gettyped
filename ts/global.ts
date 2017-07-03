type Ed = monaco.editor.IStandaloneCodeEditor;

type Global = typeof window & {
    require : any;
    __gt : {
        tsconfig : any;
        siteRoot : string;
        scrollbarSize : number;
        editors : Array<{editor : Ed; container : HTMLElement}>;
    };
};

const global : Global = window as any;

export default global;
