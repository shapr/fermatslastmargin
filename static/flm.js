document.addEventListener('DOMContentLoaded', e => {
  var params = new URLSearchParams(document.location.search)
  var pagenum = parseInt(params.get('pagenum'), 10) || 1 // this can't be a good idea
  if (pagenum < 1) {
    pagenum = 1
  }

  var content = document.getElementById('content') // I use this everywhere.
  var uid = params.get('uid') || 'todo'
  var friendview = params.get('friendview') || ''
  var didJustEscape = false
  function setAnnotation(a) {
    document.getElementById('content').value = a[0]['content'] || '' // still looks ugly to me
    document.getElementById('friend').innerHTML = a[1]['content'] || '' // save me from nulls!
  }

  // stolen from https://stackoverflow.com/a/1634841/39683
  function removeURLParameter(url, parameter) {
    //prefer to use l.search if you have a location/link object
    var urlparts = url.split('?')
    if (urlparts.length >= 2) {
      var prefix = encodeURIComponent(parameter) + '='
      var pars = urlparts[1].split(/[&;]/g)
      //reverse iteration as may be destructive
      for (var i = pars.length; i-- > 0; ) {
        //idiom for string.startsWith
        if (pars[i].lastIndexOf(prefix, 0) !== -1) {
          pars.splice(i, 1)
        }
      }
      return urlparts[0] + (pars.length > 0 ? '?' + pars.join('&') : '')
    }
    return url
  }

  function setFriends(data) {
    friends_table = document.getElementById('friends')
    $.each(data, function() {
      var tbl_row = friends_table.insertRow()
      var row_cel = tbl_row.insertCell(0)
      var ank = document.createElement('a')
      ank.title = data
      if (friendview != data) {
        ank.href = location.href + '&friendview=' + data
      } else {
        ank.href = removeURLParameter(location.href, 'friendview')
      }
      ank.appendChild(document.createTextNode(data))
      row_cel.appendChild(ank)
    })
  }

  function setPageImage(pagenum) {
    let imgsrc = '/' + uid + '/page-' + pagenum + '.png'
    document.getElementById('pageimage').setAttribute('src', imgsrc)
    return imgsrc
  }
  // from https://stackoverflow.com/a/32147146/39683
  function setHeight(jq_in) {
    jq_in.each(function(index, elem) {
      // This line will work with pure Javascript (taken from NicB's answer):
      elem.style.height = elem.scrollHeight + 'px'
    })
  }
  setHeight($('#content'))

  $(document).ready(function() {
    $('#status').val(hereagain())
    $('#pgup').val(pagenum + 1)
    $('#pgdn').val(pagenum - 1 || 1) // gosh that's ugly
    document.getElementById('uidtoupdate').value = uid
    var append = ''
    if (friendview) {
      append = '&viewfriend=' + friendview
    }
    $.get(
      '/getannotate?pagenum=' + pagenum + '&paperuid=' + uid + append,
      '',
      setAnnotation,
      'json'
    )
    $.get('/friends?paperuid=' + uid, '', setFriends, 'json')
    setPageImage(pagenum)
  })

  function hereagain() {
    return (
      window.location.protocol +
      '//' +
      window.location.host +
      window.location.pathname
    )
  }

  function buildup() {
    return hereagain() + '?pagenum=' + (pagenum - 1) + '&uid=' + uid
  }
  function builddown() {
    return hereagain() + '?pagenum=' + (pagenum + 1) + '&uid=' + uid
  }

  document.addEventListener('keydown', e => {
    if (
      e.key == 'PageDown' ||
      (e.key == 'ArrowRight' && document.activeElement != content)
    ) {
      location.href = builddown()
    }
    if (
      e.key == 'PageUp' ||
      (e.key == 'ArrowLeft' && document.activeElement != content)
    ) {
      if (pagenum >= 1) {
        location.href = buildup()
      } else {
        location.href = hereagain() + '?pagenum=1' + '&uid=' + uid
      }
    }
    if (e.key == 'Enter' && e.ctrlKey) {
      // check focus
      if (content == document.activeElement) {
        // content already has focus, CTRL+enter should submit to the server if the contents are non-empty
        // submit, check for write, if success then redirect to this page
        if (content.value.length > 0) {
          // check for empty textarea, this might work?
          // submit to server
          $.post(
            '/annotate',
            JSON.stringify({
              pageNumber: pagenum,
              content: content.value,
              paperuid: uid
            })
          )
          content.blur()
          // display saved, and drop focus
        } else {
          alert('not saving empty annotation')
        }
      } else {
        content.focus() // put focus on content
        content.style.height = content.scrollHeight + 'px'
      }
    }
    if (e.key == 'Escape') {
      content.blur()
      didJustEscape = true
    }
  })

  content.addEventListener('blur', e => {
    // check if escaped key triggered the blur, only submit if it did not.
    if (!didJustEscape) {
      didJustEscape = false
      // submit, check for write, if success then redirect to this page
      if (content.value.length > 0) {
        // check for empty textarea, this might work?
        // submit to server
        $.post(
          '/annotate',
          JSON.stringify({
            pageNumber: pagenum,
            content: content.value,
            paperuid: uid
          })
        )
      } else {
        alert('not saving empty annotation')
      }
    }
  })
})
