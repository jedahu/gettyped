export const isInView = (el : Element) => {
    const t = el.getBoundingClientRect().top;
    const b = el.getBoundingClientRect().bottom;
    const h = window.innerHeight;

    return (t >= 0 && t <= h) || (b >= 0 && b <= h) || (t <= 0 && b >= h);
};
