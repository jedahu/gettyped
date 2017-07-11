export const esc = (...s : Array<string>) : string => {
    const div = document.createElement("div");
    div.textContent = s.join("");
    return div.innerHTML;
};

export const mkElement = (...html : Array<string>) : HTMLElement => {
    const div = document.createElement("div");
    div.innerHTML = html.join("");
    return div.firstElementChild as HTMLElement;
};

