/**
 * Created by chenlingpeng on 2014/10/24.
 */

rhea = function (teamid,projectid,userid,currentuid) {
    var page = 1;
    var offset = 0;
    var pageend = false;
    var groupid = 0;
    var state = 0;
    var uid = userid;
    var canScroll = true;

    this.initvalues = function(nowstate){
        page = 1;
        offset = 0;
        pageend = false;
        state = nowstate;
        canScroll = true;
    };

    toastr.options = {
        "closeButton": false,
        "debug": false,
        "positionClass": "toast-bottom-full-width",
        "showDuration": "300",
        "hideDuration": "1000",
        "timeOut": "5000",
        "extendedTimeOut": "1000",
        "showEasing": "swing",
        "hideEasing": "linear",
        "showMethod": "fadeIn",
        "hideMethod": "fadeOut"
    };

    var states = {
        "1":"即将开始",
        "2":"正在进行",
        "3":"已完成",
        "4":"废弃"
    };


    this.init = function () {
        this.initlist();
        this.teamprojects()
    };

    this.deleteProject = function () {
        var projectid = 1;
        $.ajax({
            type: "POST",
            url: "/project/delete/" + projectid,
            dataType: 'json',
            success: function (data) {
                if (data.state == "success") {
                    alert("删除成功")
                }
                else {
                    alert("删除失败")
                }
            },
            error: function () {
                alert("删除失败")
            }
        })
    };

    itemview = function(data){
        var str = "<li><div class=\"page-header\">";
        str+="<div class='left-state1'>";
        str += "<span class=\"author_name\"><a href=\"\/others\/"+data['ownbyid']+"\">"+ data['ownby']+"</a></span>";
        str +="<span class=\"task_state\">"+data['state']+"  "+"</span>";
        str += "<span class=\"event_label pushed\">"+data['desc']+"</span>";
        str += "<span class=\"project_name\"><a href=\"\/showproject\/"+data['projectid']+"\">"+" @"+ data['projectname']+"</a></span>";
        str += "<span class=\"reply-button\">";
        str += "<button class='btn btn-primary btn-info btn-small' type='button' id='reply-state"+data['id']+"'>Messages <span class='badge'>"+data['replynum']+"</span></button>";
        str +="</span>";
        str +="</div>";
        str += "<div class=\"date\" title='"+data['ctimetitle']+"'>" + data['ctime'] + "</div>";
        str +="<div id='reply-drop-down"+data['id']+"' class='reply-down'>";
        str +="<div class='thumbnails' id='thumbnails"+data['id']+"' >";
        str +="<input class='thumbnail reply-input 'style='margin-top: 10px;' id='reply-input"+data['id']+"' placeholder='请输入对任务的回复内容'>";
        str +="<button class='btn btn-inverse btn-small reply-submit' id='reply-submit"+data['id']+"'>提交</button>";
        str +="</div>";

        str +="</div>";
        str+="</div></li>";

        return str;
    };

    this.initlist = function () {
        if(pageend) return;
        $.ajax({
            type: "POST",
            url: "/json/task/list",
            data:{page:page, uid:uid, pid:projectid, gid:groupid, state:state, offset:offset},
            dataType: 'json',
            success: function (data) {
                var arraysize=0;
                if (data) {
                    $.each(data, function (index, array){
                        if(array['ownbyid']==(currentuid+"")){

                            arraysize++;
                            var str=itemCanEdit(array);
                            $(".tasks").append(str);
                            itemevent(array);
                        }else{

                            arraysize++;
                            var str = itemview(array);
                            $(".tasks").append(str);
                            itemevent(array);
                        }
                    });
                    page++;
                    offset = data.pop()['timestamp'];
                } else {
//                    return false;
                }
                if(arraysize<15){
                    $("#nomore").show().html("已经没有更多数据。。。");
                    pageend = true;
                }
            },
            error: function () {
                toastr.error("加载失败, 请刷新后重试")
            },
            complete: function () {
            }
        })
    };

    this.scroll = function(){
        if(canScroll) {
            canScroll = false;
            var winH = $(window).height(); //页面可视区域高度
            var pageH = $(document.body).height();
            var scrollT = $(window).scrollTop(); //滚动条top
            var aa = (pageH - winH - scrollT) / winH;
            if (aa < 0.1) {
                if (pageend) return;
                $.ajax({
                    type: "POST",
                    url: "/json/task/list",
                    data: {page: page, uid: uid, pid: projectid, gid: groupid, state: state, offset: offset},
                    dataType: 'json',
                    success: function (data) {
                        var arraysize = 0;
                        if (data) {
                            $.each(data, function (index, array) {
                                if(array['ownbyid']==(currentuid+"")){
                                    arraysize++;
                                    var str=itemCanEdit(array);
                                    $(".tasks").append(str);
                                    itemevent(array);
                                }else{
                                    arraysize++;
                                    var str = itemview(array);
                                    $(".tasks").append(str);
                                    itemevent(array);
                                }

                            });
                            page++;
                            offset = data.pop()['timestamp'];
                        } else {
//                    return false;
                        }
                        if (arraysize < 15) {
                            $("#nomore").show().html("已经没有更多数据。。。");
                            pageend = true;
                        }
                    },
                    error: function () {
                        toastr.error("加载失败")
                    },
                    complete: function () {
                        canScroll=true;
                    }
                })
            } else {
                canScroll=true;
            }
        }
    };

    itemCanEdit = function(data) {
        var str = "<li  id='state"+data['id']+"'><div class=\"page-header\">";
        str +="<div class='left-state'>";
        str += "<span class=\"author_name\"><a href=\"\/others\/"+data['ownbyid']+"\">" + data['ownby'] + "</a></span>";
        str +="<span class='task_state' value='"+data['stateid']+"' id='task_state"+data['id']+"'>"+data['state']+"</span>";
        str += "<span class=\"event_label pushed\">"+data['desc']+"  "+"</span>";
        str += "<span class=\"project_name\"><a href=\"\/showproject\/"+data['projectid']+"\">"+" @"+data['projectname']+"</a></span>";


        str += "<span class=\"reply-button\">";
        str += "<button class='btn btn-primary btn-info btn-small' type='button' id='reply-state"+data['id']+"'>Messages <span class='badge'>"+data['replynum']+"</span></button>";
        str +="</span>";

        str +="</div>";

        str += "<div class=\"date\" id='date"+data['id']+"' title='" + data['ctimetitle'] + "'>" + data['ctime'] +"</div>";
        str +=  "<div class=\"icon-edit state-edit\" id='edit"+data['id']+"'></div>";



        str +="<div id='drop-down"+data['id']+"' class='drop-down'>";
        str +="<span class='edit-statetext'>修改状态：</span>";
        str +="<select class='edit-state' name='edit-statename' id='editstateid"+data['id']+"' ><option value='1'>未开始</option><option value='2'>进行中</option><option value='3'>已完成</option><option value='4'>废弃</option></select>"
        str +="<button class='btn btn-info btn-small close-state' id='close-state"+data['id']+"'>关闭</button>";
        str +="<button class='btn btn-info btn-small summit-state' id='summit-state"+data['id']+"'>提交</button>";
        str +="<button class='btn btn-success btn-small delete-state' id='delete-state"+data['id']+"'>删除当前task</button>";
        str +="<span class='editstate-tip' id='editstate-tip"+data['id']+"'></span>";
        str +="</div>";


        str +="<div id='reply-drop-down"+data['id']+"' class='reply-down'>";
        str +="<div class='thumbnails' id='thumbnails"+data['id']+"' >";
        str +="<input class='thumbnail reply-input 'style='margin-top: 10px;' id='reply-input"+data['id']+"' placeholder='请输入对任务的回复内容'>";
        str +="<button class='btn btn-inverse btn-small reply-submit' id='reply-submit"+data['id']+"'>提交</button>";
        str +="</div>";

        str +="</div>";

        str+="</div></li>";
        return str;
        $('#date'+data['id']).tooltip({ effect: "explode", duration: 1000 });
    };

    itemevent = function(data){
        var orgin=0;
        var isHidden = 1;
        $("#reply-state"+data['id']).click(function(){
            $('#reply-drop-down'+data['id']).slideToggle()
            if(isHidden==1){
                $.ajax({
                    type:"POST",
                    url:"/json/reply/list/"+data['id'],
                    data:{},
                    dataType:'json',
                    success:function(lists){
                        if (lists) {
                            $.each(lists, function (index, array) {
                                var author = "<span class=\"author_name\"><a href=\"\/others\/"+array['ownbyid']+"\">"+ array['ownby']+"</a>"+array['rtime']+"说："+"</span>";
                                var str ="<div class='thumbnail' style='margin-top: 15px;width: 450px;'>"+author+array['description']+"</div>";
                                $('#thumbnails'+data['id']).prepend(str);
                            });

                        }

                    }
                });
                isHidden=0;
            }

        });
        $("#edit"+data['id']).click(function(){
            $('#drop-down'+data['id']).slideToggle();
            orgin=$("#task_state"+data['id']).attr("value")
        });
        $('#close-state'+data['id']).click(function(){
            $('#drop-down'+data['id']).slideUp();
            $('#editstate-tip'+data['id']).text('');
        });
        $('#summit-state'+data['id']).click(function () {
            var editstateid=$('#editstateid'+data['id']).val();
            if(editstateid>orgin){
                $.ajax({
                    type:"POST",
                    url:"/json/task/modify/"+data['id'],
                    data:{editstateid:editstateid},
                    dataType:'json',
                    success:function(data2){
                        $('#drop-down'+data['id']).slideUp();
                        $('#task_state'+data['id']).html(states[editstateid]);
                        toastr.success("任务状态修改成功");

                    }
                })
            }
            else if(editstateid==orgin){
                toastr.info("任务状态没有发生改变");
            }else{
                toastr.info("任务状态不能回退");
            }
        });
        $('#reply-submit'+data['id']).click(function () {
            var reply=$('#reply-input'+data['id']).val();
            $.ajax({
                type:"POST",
                url:"/json/reply/create/"+data['id'],
                data:{reply:reply},
                dataType:'json',
                success:function(array){
                    if(array){
                        var author = "<span class=\"author_name\"><a href=\"\/others\/"+array['ownbyid']+"\">"+ array['ownby']+"</a>"+array['rtime']+"说："+"</span>";
                        var str ="<div class='thumbnail' style='margin-top: 15px;'>"+author+array['description']+"</div>";
                        $('#thumbnails'+data['id']).prepend(str);
                        toastr.success("任务回复成功");
                        $('#reply-input'+data['id']).val('');
                    }

                }
            })


        });
        $('#delete-state'+data['id']).click(function(){
            var conf=confirm("确定删除吗？");
            if(conf==true)
            {

                    var deletestateid=data['id'];

                    $.ajax({
                        type:"POST",
                        url:"/json/task/delete/"+data['id'],
                        data:{deletestateid:deletestateid},
                        dataType:'json',
                        success:function(data2){
                            if(data2==1){
                                $('#drop-down'+data['id']).slideUp();
                                $('#state'+data['id']).css('display','none');

                                toastr.success("删除任务成功");

                            }
                            else if(data2==0)
                            {
                                toastr.error("创建超过10分钟的任务无法删除");
                            }
                            else{
                                toastr.error("删除任务失败");
                            }

                        }


                    });

            }


        });

    };

    this.initlistCanEdit = function () {
        if(pageend) return;
        $.ajax({
            type: "POST",
            url: "/json/task/list",
            data:{page:page, uid:uid, pid:projectid, gid:groupid, state:state, offset:offset},
            dataType: 'json',
            success: function (data) {
                var arraysize=0;
                if (data) {
                    $.each(data, function (index, array){
                        arraysize++;
                        var str=itemCanEdit(array);
                        $(".tasks").append(str);
                        itemevent(array);
                    });
                    page++;
                    offset = data.pop()['timestamp'];
                } else {
//                    return false;
                }
                if(arraysize<15){
                    $("#nomore").show().html("已经没有更多数据。。。");
                    pageend = true;
                }
            },
            error: function () {
                $("#nomore").show().html("已经没有更多数据。。。");
                pageend = true;
            },
            complete: function () {
            }
        })
    };


    this.scrollCanEdit = function(){
        if(canScroll) {
            canScroll = false;
            var winH = $(window).height(); //页面可视区域高度
            var pageH = $(document.body).height();
            var scrollT = $(window).scrollTop(); //滚动条top
            var aa = (pageH - winH - scrollT) / winH;
            if (aa < 0.1) {
                if (pageend) return;
                $.ajax({
                    type: "POST",
                    url: "/json/task/list",
                    data: {page: page, uid: uid, pid: projectid, gid: groupid, state: state, offset: offset},
                    dataType: 'json',
                    success: function (data) {
                        var arraysize = 0;
                        if (data) {
                            $.each(data, function (index, array) {
                                arraysize++;
                                var str=itemCanEdit(array);
                                $(".tasks").append(str);
                                itemevent(array);
                            });
                            page++;
                            offset = data.pop()['timestamp'];
                        } else {
//                    return false;
                        }
                        if (arraysize < 15) {
                            $("#nomore").show().html("已经没有更多数据。。。");
                            pageend = true;
                        }
                    },
                    error: function () {
                        alert("加载失败")
                    },
                    complete: function () {
                        canScroll=true;
                    }
                })
            } else {
                canScroll=true;
            }
        }
    }

    this.teamprojects = function () {
        $.ajax({
            type: "POST",
            url: "/json/project/list",
            data: {tid: teamid},
            dataType: 'json',
            success: function (data) {
                if (data) {
                    $.each(data, function (index, array) {
                        // TODO: 展示到页面
                        var str = "<li><div class='event-title thumbnail' id='thumbnail"+array['id']+"'>";
                        str += "<span class=\"project_name \"><a href=\"\/showproject\/"+array['id']+"\">"+ array['name']+"</a></span>";
                        str += "<span class='icon-chevron-right'></span>";
                        str += "</div></li>";
                        $("#projects").append(str);
                        $("#thumbnail"+array['id']).click(function(){
                            window.location.href="/showproject/"+array['id'];
                        })
                    });
                } else {
                    return false;
                }
            },
            error: function () {
                return false;
            }
        })
    };

    this.teamsdetail = function(){
        $.ajax({
            type: "POST",
            url: "/json/project/list",
            data: {tid: teamid},
            dataType: 'json',
            success: function (data) {
                if (data) {
                    $.each(data, function (index, array) {
                        // TODO: 展示到页面
                        var str = "<span><h1><small><a href='/showproject/"+array['id']+"'>"+array['name']+"</a></small></h1></span>";
                        str += "<span style='color: darkolivegreen;font-weight: bold;font-size: 16px;'>"+array['des']+"</span><span style='margin-left: 20px;;'><small>负责人：<a href='/others/"+array['ownbyid']+"'> "+array['ownby']+"</small></span>";
                        str += "<div class=\"page-header\"></div>";
                        $("#projects").append(str);
                    });
                } else {
                    return false;
                }
            },
            error: function () {
                return false;
            }
        })
    };

    this.createTask = function () {
        var pid = $("#projectid").val();
        var task = $("#newtask").val();
        var stateid=$("#stateid").val();
        if(task.replace(/(^s*)|(s*$)/g, "").length > 0){
            $.ajax({
                type: "POST",
                url: "/json/task/create",
                data: {pid: pid, desc: task, stateid:stateid},
                dataType: 'json',
                success: function (data) {
                    if (data) {
                        var str=itemCanEdit(data);

                        if(state=="0" || state== data['stateid']){
                            $(str).hide().prependTo('.tasks').slideDown('slow');

                        }
                        itemevent(data);
                        $("#newtask").val('');

                        if(offset == 0){
                            offset = data['timestamp'];
                        }
                        toastr.success("任务创建成功")
                    } else {
                        return false;
                    }
                },
                error: function () {
                    toastr.error("任务创建失败,项目名称任务状态为必选");
                    return false;
                }
            })
        }

    };

    this.createProject = function () {
        var ownby = $("#ownby").val();
        var projectdesc = $("#projectdesc").val();
        var projectname = $("#projectname").val();
        var num=0;
        var othermembers=[];
        var nonempty;
        $('#pre-selected-options option:selected').each(function(){


                othermembers[num++]=$(this).val();

        });
        if(num==0){
            nonempty=0;
        }
        else
        {
            nonempty=1;
        }
        if(projectdesc.replace(/(^s*)|(s*$)/g, "").length > 0 && projectname.replace(/(^s*)|(s*$)/g, "").length > 0){
            $.ajax({
                type: "POST",
                url: "/json/project/create",
                data: {ownby: ownby,
                    projectdesc: projectdesc,
                    projectname: projectname,
                    othermembers: othermembers,
                    nonempty:nonempty},
                dataType: 'json',
                success: function (data) {
                    if (data) {
                        var str = "<div class=\"page-header\"><h1><small>"+data['name']+"</small></h1></div>";
                        str += "<p>"+data['des']+"</p>";
                        str += "<div class=\"page-header\"><h1><small>负责人："+data['ownby']+"</small></h1></div>";
                        $("#projects").prepend(str);

                        $("#projectname").val('');
                        $("#projectdesc").val('');
                        toastr.success("项目创建成功");
                    } else {
                        return false;
                    }
                },
                error: function () {
                    toastr.error("任务创建失败");
                    return false;
                }
            })
        }

    }

    this.modifyProject = function(projectid){
        var ownby=$("#owner").val();
        var pstate=$("#pstate").val();

        var projectdesc = $("#editprojectdesc").val();
        var num=0;
        var othermem=[];
        var nonempty;

        $('#pre-selected-options option:selected').each(function(){


            othermem[num++]=$(this).val();

        });
        if(num==0)
        {
            nonempty=0;
        }
        else
        {
            nonempty=1;
        }

        if(projectdesc.replace(/(^s*)|(s*$)/g, "").length > 0 ){
            $.ajax({
                type: "POST",
                url: "/json/project/modify/"+projectid,
                data: {ownby:ownby,pstate:pstate, projectdesc: projectdesc,othermem:othermem,nonempty:nonempty},
                dataType: 'json',
                success: function (data) {
                    if (data) {
                        $("#projectdesc").html(data['des']);
                        $("#ownby").html("负责人："+data['ownby']);
                        toastr.success("项目修改成功");
                    } else {
                        return false;
                    }
                },
                error: function () {
                    toastr.error("项目修改失败");
                    return false;
                }
            })
        }
    }
};


