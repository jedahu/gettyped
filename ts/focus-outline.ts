// Adapted from Chromium's focus_outline_manager.js

// Copyright (c) 2012 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// Set `className` on HTML element of `doc` when TAB is pressed. Remove when
// mouse is clicked.
export const manageFocusOutlines = (doc : HTMLDocument, className : string) => {
    let show = true;
    const docClasses = doc.documentElement.classList;
    const showOutline = (b : boolean) => () => { show = b; };
    const focusOut = () => window.setTimeout(() => {
        if (!doc.hasFocus()) {
            show = true;
            updateVisibility();
        }
    }, 0);
    const updateVisibility = () => docClasses.toggle(className, show);

    doc.addEventListener("keydown", showOutline(true), true);
    doc.addEventListener("mousedown", showOutline(false), true);
    doc.addEventListener("focus", updateVisibility, true);
    doc.addEventListener("focusout", focusOut, true);
    updateVisibility();
};
