//Auto hide tab bar only when Tree Style Tab sidebar is selected and visible.
// https://github.com/tkhquang/userChrome.js/blob/master/TST_tabbar.uc.js

(function () {
    "use strict";

    const tabbar = document.getElementById("TabsToolbar");
    const navbar = document.getElementById("nav-bar");
    const navbox = document.getElementById("navigator-toolbox");
    function showHideTabbar() {
        const sidebarBox = document.getElementById("sidebar-box");
        const sidebarCommand = sidebarBox.getAttribute("sidebarcommand");
        navbox.style.marginLeft = ( sidebarBox.hidden ? "0" : sidebarBox.width + "px" );
        navbar.style.marginRight = (
            (!sidebarBox.hidden && sidebarCommand === "treestyletab_piro_sakura_ne_jp-sidebar-action") ? "0" : "60vw");
        tabbar.style.opacity = (
            (!sidebarBox.hidden && sidebarCommand === "treestyletab_piro_sakura_ne_jp-sidebar-action") ? "0" : "1");
      sidebarBox.style.marginTop = "-" + navbox.clientHeight + "px";
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
