const vars : {[prop : string] : string | number} = {
    navBarHeight: "50px",
    navDrawerWidth: "26ex",
    mainLeftMargin: "30ex",
    mainMinWidth: "50ex"
};

export default vars;

export const asString = () =>
    Object.keys(vars).
        map(k => `$gt-${k}: ${vars[k]};`).
        join("\n");
