# HtmlEmailBuilder
A set of Delphi interfaces for building HTML email templates 

I've added a simple demo project showing a few basic examples.

### Note

The code is not perfect by any stretch of the imagination.  It was more a proof of concept which should save me a bit of time with my main project.

I've only added a subset of elements and CSS styles currently as this is all I've needed currently.  I'll add more as I get a chance, they are very easy to add.

As an example of modifying a CSS property,  here we add an image and then set the width to 32px...
```
AHtml.Content.AddImage(Application.Icon, alCenter).Style.Width := '32px';
```
And here we can override an H1 elements color and set it to red...
```
AHtml.Content.AddH1('Hello!).Style.FontColor := 'red';
```
Any questions, let me know.
