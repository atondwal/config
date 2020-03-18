//Auto hide tab bar only when Tree Style Tab sidebar is selected and visible.
// https://github.com/tkhquang/userChrome.js/blob/master/TST_tabbar.uc.js

(function () {
    "use strict";

    const tabbar = document.getElementById("TabsToolbar");
    const navbar = document.getElementById("nav-bar");
    const navbox = document.getElementById("navigator-toolbox");
    function showHideTabbar() {
        const urlbarView = document.getElementsByClassName("urlbarView")[0];
        const sidebarBox = document.getElementById("sidebar-box");
        const sidebarCommand = sidebarBox.getAttribute("sidebarcommand");
        navbox.style.marginLeft = ( sidebarBox.hidden ? "0" : sidebarBox.width + "px" );
        tabbar.style.display = (
            (!sidebarBox.hidden && sidebarCommand === "treestyletab_piro_sakura_ne_jp-sidebar-action") ? "none" : "-moz-box");
        // tabbar.style.marginLeft = ( navbox.clientWidth <= 800 ? "0" : "40vw");
        navbar.style.marginRight = (
            (!sidebarBox.hidden && sidebarCommand === "treestyletab_piro_sakura_ne_jp-sidebar-action"
            ) ? "0" : (navbox.clientWidth <= 800 ? "0" : "60vw"));
        navbar.style.marginTop = (
            ((!sidebarBox.hidden && sidebarCommand === "treestyletab_piro_sakura_ne_jp-sidebar-action")
            ) ? "0" : (navbox.clientWidth <= 800 ? "0" : "-20px") );
        sidebarBox.style.marginTop = "-" + navbox.clientHeight + "px";
        sidebarBox.style.setProperty('--thin-tab-width', Math.min(sidebarBox.width, 200) + "px");
    }
    const observer = new MutationObserver(showHideTabbar);
    observer.observe(document.getElementById("sidebar-box"), {
        attributes: true,
        attributeFilter: [
            "sidebarcommand",
            "width",
            "hidden"]
    });
}());
