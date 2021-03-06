/*
This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

https://www.gnu.org/licenses/
*/


//= require_tree .

/* Link event binding code. */
// Given a node, finds all <a> tags and mocks them
mockNode = function (node) {
    links = node.getElementsByTagName("a");
    mockCollection(links);
}

// Given an HTMLCollection, mocks the localized links
mockCollection = function (collection) {
    links = [];
    // Turn collection into an array for easy filter/map
    for (var ii = 0; ii < collection.length; ii++) {
        links[ii] = collection[ii];
    }
    links.filter(referencesLocal).map(mockLink);
}

// Given an <a>, binds navigateTo and preventDefault, marks link as mocked
mockLink = function (a) {
    a.addEventListener("click", prevDefaultCallback);
    a.addEventListener("click", navigateTo.bind(window, a.href));
    a.setAttribute("data-mocked", "true");
}
prevDefaultCallback = function (e) { e.preventDefault(); }

// Check if an <a> node references a local path and hasn't been mocked
// mocking is the act by which we take a link and suppress it with JS
referencesLocal = function (a) {
    // Use a.getAttribute instead of a.href since a.href coerces to full url
    url = a.getAttribute("href");
    
    // If the link has no href, ignore it
    if (url == undefined) return false;

    // If the link has mocking disabled, ignore it
    if (a.getAttribute("data-disable-mocking") == "true") return false;

    // If the link has already been mocked
    if (a.getAttribute("data-mocked") == "true") return false;

    // If the link has a URI scheme, consider it non-local
    hasScheme = (/^\w+:\/\//).test(url)
    return !hasScheme;
}

/* History state changers and handlers */
navigateTo = function (url) {
    history.pushState({}, "", url);
    onNavigate();
} 
onNavigate = function () {
    showCurrentPage();
}
window.onpopstate = onNavigate;

// Main entry point for a url change (popstate) event
// mediates between all associated content getting and embedding
showCurrentPage = function () {
    var path = window.location.pathname;

    // If we are navigating to root, simply close the current page and stop.
    if (path === "/") {
        var profile = document.getElementById("profile");
        profile.style.backgroundImage = "url(<%= asset_path 'profile.jpg' %>)";
        // Add small delay to allow profile to be ready to flex before content
        // closes and blurb fades in
        setTimeout(function () {
            document.body.classList.remove("open");
            setTitleFromSection();
        }, 20);
        return;
    }

    var section = findSection(path);
    if (section == undefined) {
        console.log("section does not exist", path)
        clearSelection();
        document.body.classList.add("open");
        getContent(path, function (content) {
            var section = createSection(path, content);
            setTitleFromSection(section);
            setTimeout(selectSection.bind(window, path), 100);
        });
    } else {
        var section = selectSection(path);
        setTitleFromSection(section);
        document.body.classList.add("open");
    }
}

setTitleFromSection = function (section) {
    if (section == undefined) return document.title = "Dylan Thinnes";

    var newTitle = section.getElementsByTagName("title")[0];
    if (newTitle == undefined) return document.title = "Dylan Thinnes";
    
    return document.title = newTitle.innerHTML;
}

/* URL getting and replacement */
// gets the content at the endpoint without the layout
getContent = function (url, callback) {
    var req = new XMLHttpRequest();
    req.onreadystatechange = function () {
        if (req.readyState == 4) {
            callback(req.response);
        }
    }
    req.open("GET", url + "?nolayout=true")
    req.send();
}

/* Section Management */
// Sections contain the content for a given endpoint and have a data-url attr
// The page may be initialized with any number of sections

// Finds the section corresponding to a given url
findSection = function (url) {
    var sections = document.getElementsByClassName("section");
    for (var ii = 0; ii < sections.length; ii++) {
        var section = sections[ii];
        var sectionUrl = section.getAttribute("data-url");
        if (sectionUrl == url) return section;
    }
}

// create a section container for content retrieved from an endpoint
// append it to the page container
createSection = function (url, content) {
    var section = document.createElement("div");
    section.classList.add("section");
    section.innerHTML = content;
    section.setAttribute("data-url", url);

    var loading = document.getElementById("loading");

    document.getElementById("content").insertBefore(section, loading);
    mockNode(section);

    return section;
}

// select a section
selectSection = function (url) {
    clearSelection();
    section = findSection(url);
    section.classList.add("selected");
    return section;
}

// unselect all sections
clearSelection = function (url) {
    var sections = document.getElementsByClassName("section");
    for (var ii = 0; ii < sections.length; ii++) {
        sections[ii].classList.remove("selected");
    }
}

// Initialize mocking of links in page.
mockNode(document);
console.log("Nodes mocked.")
