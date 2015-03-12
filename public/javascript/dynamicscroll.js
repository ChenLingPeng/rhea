/**
 * Created by chenlingpeng on 2014/10/23.
 */


$(function () {
    var winH = $(window).height(); //页面可视区域高度
    var i = 1; //设置当前页数
//    alert("register");
    $(window).scroll(function () {
//        alert("scroll");
        var pageH = $(document.body).height();
        var scrollT = $(window).scrollTop(); //滚动条top
        var aa = (pageH - winH - scrollT) / winH;
        if (aa < 0.02) {
//            alert("register2");
            $.ajax({
                url: "scroll",
                data: {page: i},
                type: 'post',
                dataType: 'json',
                success: function (data) {
                    if (data) {
                        $.each(data, function (index, array) {
                            var str = "<div class=\"single_item\"><div class=\"element_head\">";
                            str += "<div class=\"date\">" + array['date'] + "</div>";
                            str += "<div class=\"author\">" + array['author'] + "</div>";
                            str += "</div><div class=\"content\">" + array['content'] + "</div></div>";
                            $("#container").append(str);
                        });
                        i++;
                    } else {
                        $(".nodata").show().html("别滚动了，已经到底了。。。");
                        return false;
                    }
                },
                error: function () {
                    $(".nodata").show().html("别滚动了，已经到底了。。。");
                    return false;
                }
            });

            $.getJSON("scroll", {page: i}, function (json) {
                if (json) {
                    var str = "";
                    $.each(json, function (index, array) {
                        var str = "<div class=\"single_item\"><div class=\"element_head\">";
                        str += "<div class=\"date\">" + array['date'] + "</div>";
                        str += "<div class=\"author\">" + array['author'] + "</div>";
                        str += "</div><div class=\"content\">" + array['content'] + "</div></div>";
                        $("#container").append(str);
                    });
                    i++;
                } else {
                    $(".nodata").show().html("别滚动了，已经到底了。。。");
                    return false;
                }
            });
        }
    });
});