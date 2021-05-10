from operator import itemgetter

def get_dominate_topic(top_pors):
    top_pors_nums = [float(t) for t in top_pors]
    top_and_top_por = []
    i = 0
    for t in top_pors_nums:
        top_and_top_por.append((i, t))
        i+=1
    dom = max(top_and_top_por,key=itemgetter(1))[0] + 1
    return dom

# def get_county():
#     id_to_county = {}
#     fn = "metadata_perarticle_withcorpus_filtered_withdaynum_withlogodds.csv"
#     with open(fn) as data:
#         for line in data:
#             l = line.strip().split(",")
#             id = l[0]
#             county = l[11]
#             id_to_county[id] = county
#     return id_to_county

fn = "theta_Feb27update.csv"
outfn = "theta_Feb27update_withdomtopic.csv"

#id_to_county = get_county()
with open(outfn, "w") as out:
    with open(fn) as data:
        header = data.readline().strip().split(",")
        new_header = "id,sourcedomain_id,dominate_topic," + ",".join(header[2:])
        out.write(new_header+"\n")
        for line in data:
            l = line.strip().split(",")
            id = l[1]
            #county = id_to_county[id]
            #date = "-".join((id.split("-")[0], id.split("-")[1], id.split("-")[2]))
            #source = id.split("-")[3].split("_")[0]
            sourcedomain = id.split("-2020")[0]
            dom = get_dominate_topic(l[2:])
            outl = ",".join((id, sourcedomain, str(dom), ",".join(l[2:])))
            out.write(outl+"\n")
